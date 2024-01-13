{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Local.Prelude

import Local.Control.Monad
import qualified Local.Graphics.UI.GLFW as GLFW
import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import ApplicationException
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Bits
import Data.List
import Data.Function
import Data.Functor
import Data.IORef
import Foreign.C.String hiding (withCString, peekCString)
import Foreign.Marshal.Array
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
        withNextPtr = return nullPtr,
        flags = 0,

        withAppInfoPtr =
          allocaMarshal ApplicationInfo {
            withAppNamePtr = return (Ptr "Vulkan Sandbox"#),
            appVersion = VK_MAKE_VERSION 1 0 0,
            withEngineNamePtr = return nullPtr,
            engineVersion = 0,
            apiVersion = VK_API_VERSION_1_0
          },

        withEnabledLayerNamesPtrLen = do
          (numLayers, layersPtr) <- Codensity $ withContUncurried (withArrayLen validationLayers)
          return (layersPtr, fromIntegral numLayers),

        withEnabledExtensionNamesPtrLen = do
          (numExtensions, extensionsPtr) <- Codensity $ withContUncurried (withArrayLen requiredInstanceExtensions)
          return (extensionsPtr, fromIntegral numExtensions)
      }
    )
    (return nullPtr)
    (return nullPtr)
  putStrLn "Vulkan instance created."

#ifndef ndebug
  debugUtilsExt <- getInstanceExtension @VK_EXT_debug_utils vkInstance

  debugMessengerCallbackFunPtr <- acquireIn mainScope $
    haskellFunPtrResource wrapPFN_vkDebugUtilsMessengerCallbackEXT handleDebugMessage

  void . acquireIn mainScope $ debugUtilsMessengerResource debugUtilsExt vkInstance
    (
      allocaMarshal DebugUtilsMessengerCreateInfo {
        withNextPtr = return nullPtr,
        messageSeverity =
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT,
        messageType =
          VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT .|.
          VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
        userCallbackFunPtr = debugMessengerCallbackFunPtr,
        withUserDataPtr = (return nullPtr)
      }
    )
    (return nullPtr)
    (return nullPtr)
  putStrLn "Debug messenger callback registered."
#endif

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
#ifndef ndebug
  VK_EXT_DEBUG_UTILS_EXTENSION_NAME :
#endif
  [
  ]

validationLayers :: [CString]
validationLayers =
#ifndef ndebug
  Ptr "VK_LAYER_KHRONOS_validation"# :
#endif
  [
  ]

handleDebugMessage :: PFN_vkDebugUtilsMessengerCallbackEXT
handleDebugMessage messageSeverity messageTypes callbackDataPtr userDataPtr = do
  putStrLn "--Debug Utils Message--"
  putStrLn $ "Severity: " ++ show messageSeverity
  putStrLn $ "Types: " ++ show messageTypes
  runForPtr callbackDataPtr do
    messageIdNamePtr <- castPtr <$> peekPtrOffset @"pMessageIdName"
    liftIO $ do
      messageIdName <- peekCString utf8 messageIdNamePtr
      putStrLn $ "Message ID Name: " ++ messageIdName
    messageIdNumber <- peekPtrOffset @"messageIdNumber"
    liftIO $ putStrLn $ "Message ID Number: " ++ show messageIdNumber
    messagePtr <- castPtr <$> peekPtrOffset @"pMessage"
    liftIO $ do
      message <- peekCString utf8 messagePtr
      putStrLn $ "Message: " ++ message
    queueLabelCount <- peekPtrOffset @"queueLabelCount"
    when (queueLabelCount > 0) do
      queueLabelsPtr <- peekPtrOffset @"pQueueLabels"
      liftIO $ do
        putStrLn "Queue Labels:"
        forM_ [0..fromIntegral queueLabelCount] \idx -> do
          labelNamePtr <- runForPtr (advancePtr queueLabelsPtr idx) (peekPtrOffset @"pLabelName")
          labelName <- peekCString utf8 (castPtr labelNamePtr)
          putStrLn $ "  " ++ labelName
    cmdBufLabelCount <- peekPtrOffset @"cmdBufLabelCount"
    when (cmdBufLabelCount > 0) do
      cmdBufLabelsPtr <- peekPtrOffset @"pCmdBufLabels"
      liftIO $ do
        putStrLn "Command Buffer Labels:"
        forM_ [0..fromIntegral cmdBufLabelCount] \idx -> do
          labelNamePtr <- runForPtr (advancePtr cmdBufLabelsPtr idx) (peekPtrOffset @"pLabelName")
          labelName <- peekCString utf8 (castPtr labelNamePtr)
          putStrLn $ "  " ++ labelName
    objectCount <- peekPtrOffset @"objectCount"
    when (objectCount > 0) do
      objectsPtr <- peekPtrOffset @"pObjects"
      liftIO $ do
        putStrLn "Objects:"
        forM_ [0..fromIntegral objectCount] \idx -> do
          (objectType, objectHandle, objectNamePtr) <-
            runForPtr (advancePtr objectsPtr idx) $
              (,,) <$>
              peekPtrOffset @"objectType" <*>
              peekPtrOffset @"objectHandle" <*>
              peekPtrOffset @"pObjectName"
          objectName <- if objectNamePtr /= nullPtr then peekCString utf8 (castPtr objectNamePtr) else return ""
          putStrLn $ "  " ++ show objectType ++ " " ++ show objectHandle ++ " " ++ objectName
  putStrLn ""
  return VK_FALSE
