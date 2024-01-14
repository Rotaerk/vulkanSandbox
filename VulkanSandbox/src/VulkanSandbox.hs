{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Local.Control.Monad
import Local.Control.Monad.Codensity
import qualified Local.Graphics.UI.GLFW as GLFW
import Local.Foreign.Marshal.Array
import Local.Foreign.Ptr

import ApplicationException
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Bits
import Data.Function
import Data.Functor
import Data.IORef
import Foreign.C.String hiding (withCString, peekCString)
import Foreign.Storable
import Foreign.Storable.Offset
import GHC.Foreign
import GHC.Ptr
import MarshalAs
import ScopedResource
import System.Clock
import System.IO
import Vulkan.Auxiliary
import Vulkan.Auxiliary.Ext.VK_EXT_debug_utils

main :: IO ()
main =
  mainBody
  `catch` (
    \(e :: ApplicationException) ->
      putStrLn $ displayException e
  )
  `catch` (
    \(e :: GLFW.GLFWException) ->
      putStrLn $ displayException e
  )

mainBody :: IO ()
mainBody = withNewScope \mainScope -> do
  GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
    GLFW.throwGLFWExceptionM ("GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage)
  putStrLn "GLFW error callback set."

  acquireIn mainScope GLFW.initializationResource
  putStrLn "GLFW initialized"

  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  window <- acquireIn mainScope $ GLFW.windowResource 800 600 "Vulkan Sandbox" Nothing Nothing
  putStrLn "GLFW window created."

  lastWindowResizeTimeRef <- newIORef Nothing
  GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
    time <- getTime Monotonic
    writeIORef lastWindowResizeTimeRef $ Just time
  putStrLn "Window framebuffer size callback registered."

  requiredInstanceExtensions <- (appInstanceExtensions ++) <$> liftIO GLFW.getRequiredInstanceExtensions
  putStrLn "Identified required vulkan extensions."

  vkInstance <- acquireIn mainScope $ instanceResource
    (
      allocaMarshal InstanceCreateInfo {
        withNextPtr = pure nullPtr,
        flags = 0,

        withAppInfoPtr =
          Codensity $ allocaMarshal ApplicationInfo {
            withAppNamePtr = pure (Ptr "Vulkan Sandbox"#),
            appVersion = VK_MAKE_VERSION 1 0 0,
            withEngineNamePtr = pure nullPtr,
            engineVersion = 0,
            apiVersion = VK_API_VERSION_1_0
          },

        withEnabledLayerNamesPtrLen = do
          (numLayers, layersPtr) <- Codensity $ withArrayLenTuple validationLayers
          pure (layersPtr, fromIntegral numLayers),

        withEnabledExtensionNamesPtrLen = do
          (numExtensions, extensionsPtr) <- Codensity $ withArrayLenTuple requiredInstanceExtensions
          pure (extensionsPtr, fromIntegral numExtensions)
      }
    )
    ($ nullPtr)
    ($ nullPtr)
  putStrLn "Vulkan instance created."

#ifndef NDEBUG
  debugUtilsExt <- getInstanceExtension @VK_EXT_debug_utils vkInstance

  debugMessengerCallbackFunPtr <- acquireIn mainScope $
    haskellFunPtrResource wrapPFN_vkDebugUtilsMessengerCallbackEXT handleDebugMessage

  void . acquireIn mainScope $ debugUtilsMessengerResource debugUtilsExt vkInstance
    (
      allocaMarshal DebugUtilsMessengerCreateInfo {
        withNextPtr = pure nullPtr,
        messageSeverity =
          --VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT .|.
          --VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        messageType =
          VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        userCallbackFunPtr = debugMessengerCallbackFunPtr,
        withUserDataPtr = pure nullPtr
      }
    )
    ($ nullPtr)
    ($ nullPtr)
  putStrLn "Debug messenger callback registered."
#endif

  withEnumeratedPhysicalDevices vkInstance (allocaArray . fromIntegral) \(physDevicesCount, physDevicesPtr) -> do
    physDevices <- peekArray (fromIntegral physDevicesCount) physDevicesPtr
    forM_ physDevices \physDevice ->
      withPhysicalDeviceProperties physDevice \propertiesPtr ->
      withPhysicalDeviceMemoryProperties physDevice \memoryPropertiesPtr -> do
        deviceType <- peek $ offset @"deviceType" propertiesPtr
        memoryHeapCount <- peek $ offset @"memoryHeapCount" memoryPropertiesPtr
        putStrLn "--Phys Device--"
        putStrLn $ "Type: " ++ show deviceType
        putStrLn $ "Memory Heap Count: " ++ show memoryHeapCount

  putStrLn "Render loop starting."
  fix $ \renderLoop ->
    GLFW.getWindowStatus window lastWindowResizeTimeRef >>= \case
      GLFW.WindowResized -> do
        (width, height) <- GLFW.getFramebufferSize window
        renderLoop
      GLFW.WindowClosed -> pure ()
      GLFW.WindowReady -> do
        whenM (GLFW.getKey window GLFW.Key'Escape <&> (== GLFW.KeyState'Pressed)) $
          GLFW.setWindowShouldClose window True
        renderLoop

appInstanceExtensions :: [CString]
appInstanceExtensions =
#ifndef NDEBUG
  VK_EXT_DEBUG_UTILS_EXTENSION_NAME :
#endif
  [
  ]

validationLayers :: [CString]
validationLayers =
#ifndef NDEBUG
  Ptr "VK_LAYER_KHRONOS_validation"# :
#endif
  [
  ]

handleDebugMessage :: PFN_vkDebugUtilsMessengerCallbackEXT
handleDebugMessage messageSeverity messageTypes callbackDataPtr userDataPtr = do
  putStrLn "--Debug Utils Message--"
  putStrLn $ "Severity: " ++ show messageSeverity
  putStrLn $ "Types: " ++ show messageTypes
  let ptr = callbackDataPtr
  messageIdNamePtr <- peek (offset @"pMessageIdName" ptr)
  liftIO $ do
    messageIdName <- peekCString utf8 (castPtr messageIdNamePtr)
    putStrLn $ "Message ID Name: " ++ messageIdName
  messageIdNumber <- peek $ offset @"messageIdNumber" ptr
  liftIO $ putStrLn $ "Message ID Number: " ++ show messageIdNumber
  messagePtr <- peek $ offset @"pMessage" ptr
  liftIO $ do
    message <- peekCString utf8 (castPtr messagePtr)
    putStrLn $ "Message: " ++ message
  queueLabelCount <- peek (offset @"queueLabelCount" ptr)
  when (queueLabelCount > 0) do
    queueLabelsPtr <- peek (offset @"pQueueLabels" ptr)
    liftIO $ do
      putStrLn "Queue Labels:"
      forM_ (arrayElemPtrs queueLabelsPtr (fromIntegral queueLabelCount)) \labelPtr -> do
        labelNamePtr <- peek $ offset @"pLabelName" labelPtr
        labelName <- peekCString utf8 (castPtr labelNamePtr)
        putStrLn $ "  " ++ labelName
  cmdBufLabelCount <- peek (offset @"cmdBufLabelCount" ptr)
  when (cmdBufLabelCount > 0) do
    cmdBufLabelsPtr <- peek (offset @"pCmdBufLabels" ptr)
    liftIO $ do
      putStrLn "Command Buffer Labels:"
      forM_ (arrayElemPtrs cmdBufLabelsPtr (fromIntegral cmdBufLabelCount)) \labelPtr -> do
        labelNamePtr <- peek $ offset @"pLabelName" labelPtr
        labelName <- peekCString utf8 (castPtr labelNamePtr)
        putStrLn $ "  " ++ labelName
  objectCount <- peek $ offset @"objectCount" ptr
  when (objectCount > 0) do
    objectsPtr <- peek $ offset @"pObjects" ptr
    liftIO $ do
      putStrLn "Objects:"
      forM_ (arrayElemPtrs objectsPtr (fromIntegral objectCount)) \objectPtr -> do
        objectType <- peek $ offset @"objectType" objectPtr
        objectHandle <- peek $ offset @"objectHandle" objectPtr
        objectNamePtr <- peek $ offset @"pObjectName" objectPtr
        objectName <- if objectNamePtr /= nullPtr then peekCString utf8 (castPtr objectNamePtr) else pure ""
        putStrLn $ "  " ++ show objectType ++ " " ++ show objectHandle ++ " " ++ objectName
  putStrLn ""
  pure VK_FALSE

pickPhysDevice :: VkInstance -> IO VkPhysicalDevice
pickPhysDevice vkInstance =
  withEnumeratedPhysicalDevices vkInstance (allocaArray . fromIntegral) \(physDevicesCount, physDevicesPtr) -> do
    physDevices <- peekArray (fromIntegral physDevicesCount) physDevicesPtr
    forM_ physDevices \physDevice -> do
      deviceType <- withPhysicalDeviceProperties physDevice \ptr -> do
        peek $ offset @"deviceType" ptr
      memoryHeapCount <- withPhysicalDeviceMemoryProperties physDevice \ptr -> do
        peek $ offset @"memoryHeapCount" ptr
      putStrLn "--Phys Device--"
      putStrLn $ "Type: " ++ show deviceType
      putStrLn $ "Memory Heap Count: " ++ show memoryHeapCount
    return undefined

{-
pickPhysicalDevice :: VkInstance -> IO VkPhysicalDevice
pickPhysicalDevice vkInstance = lowerCodensity do
  (physDevicesCount, physDevicesPtr) <- Codensity $ withEnumeratedPhysicalDevices vkInstance \count -> allocaArray (fromIntegral count)
  liftIO $ withNewScope \physDevSelectionScope -> do
    physDevices <- peekArray (fromIntegral physDevicesCount) physDevicesPtr
    pdInfos <- forM physDevices $ \physDevice -> do
      (pdpPtr, pdpReleaseHandle) <- tentativelyAcquireIn physDevSelectionScope (mallocResource @VkPhysicalDeviceProperties)
      vkGetPhysicalDeviceProperties physDevice pdpPtr
      (pdmpPtr, pdmpReleaseHandle) <- tentativelyAcquireIn physDevSelectionScope (mallocResource @VkPhysicalDeviceMemoryProperties)
      vkGetPhysicalDeviceMemoryProperties physDevice pdmpPtr
      (pdfPtr, pdfReleaseHandle) <- tentativelyAcquireIn physDevSelectionScope (mallocResource @VkPhysicalDeviceFeatures)
      vkGetPhysicalDeviceFeatures physDevice pdfPtr
      pure (physDevice, pdpPtr, pdpReleaseHandle, pdmpPtr, pdmpReleaseHandle, pdfPtr, pdfReleaseHandle)
    pure undefined
-}
