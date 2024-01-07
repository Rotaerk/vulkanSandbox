{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Local.Prelude

import qualified Local.Graphics.UI.GLFW as GLFW
import Local.Foreign.Ptr

import ApplicationException
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Bits
import Data.Function
import Data.Functor
import Data.IORef
import Foreign.C.String hiding (withCString)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import GHC.Foreign hiding (peekCString)
import GHC.Ptr
import MarshalAs
import ScopedResource
import System.Clock
import System.IO
import Vulkan.Auxiliary
import Vulkan.Auxiliary.Ext.VK_EXT_debug_report
import Data.Word

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

  vkInstance <- acquireIn mainScope $ vkInstanceResource
    (
      allocaMarshal VkInstanceCreateInfoFields {
        icif'withNextPtr = return nullPtr,
        icif'flags = 0,

        icif'withAppInfoPtr =
          allocaMarshal VkApplicationInfoFields {
            aif'withAppNamePtr = return (Ptr "Vulkan Sandbox"#),
            aif'appVersion = VK_MAKE_VERSION 1 0 0,
            aif'withEngineNamePtr = return nullPtr,
            aif'engineVersion = 0,
            aif'apiVersion = VK_API_VERSION_1_0
          },

        icif'withEnabledLayerNamesPtrLen = do
          (numLayers, layersPtr) <- Codensity $ withContUncurried (withArrayLen validationLayers)
          return (layersPtr, fromIntegral numLayers),

        icif'withEnabledExtensionNamesPtrLen = do
          (numExtensions, extensionsPtr) <- Codensity $ withContUncurried (withArrayLen requiredInstanceExtensions)
          return (extensionsPtr, fromIntegral numExtensions)
      }
    )
    (return nullPtr)
    (return nullPtr)
  putStrLn "Vulkan instance created."

#ifndef NDEBUG
  debugReportExt <- getVkInstanceExtension @VK_EXT_debug_report vkInstance

  let
    layerNamePtr = Ptr "MY LAYER"#
    messagePtr = Ptr "MY MESSAGE"#
    userDataPtr = Ptr "MY USER DATA"#

  putStrLn $ "Layer Name Param Ptr:" ++ show layerNamePtr
  putStrLn $ "Message Param Ptr:" ++ show messagePtr
  putStrLn $ "User Data Ptr:" ++ show userDataPtr

  debugCallbackFunPtr <- acquireIn mainScope $ haskellFunPtrResource wrapPFN_vkDebugReportCallbackEXT
    \flags objectType object location messageCode layerPrefixPtr messagePtr userDataPtr -> do
      layerPrefix <- peekCString (castPtr layerPrefixPtr)
      message <- peekCString (castPtr messagePtr)
      userDataStr <- peekCString (castPtr userDataPtr)
      putStrLn "Debug report:"
      putStrLn $ "Flags=" ++ show flags
      putStrLn $ "ObjectType=" ++ show objectType
      putStrLn $ "Object=" ++ show object
      putStrLn $ "Location=" ++ show location
      putStrLn $ "MessageCode=" ++ show messageCode
      putStrLn $ "LayerPrefixPtr=" ++ show layerPrefixPtr
      putStrLn $ "MessagePtr=" ++ show messagePtr
      putStrLn $ "UserDataPtr=" ++ show userDataPtr
      putStrLn ""
      return VK_FALSE

  void . acquireIn mainScope $ vkDebugReportCallbackEXTResource debugReportExt
    (
      allocaMarshal VkDebugReportCallbackCreateInfoEXTFields {
        drcci'withNextPtr = return nullPtr,
        drcci'flags =
          VK_DEBUG_REPORT_ERROR_BIT_EXT .|.
          VK_DEBUG_REPORT_WARNING_BIT_EXT .|.
          VK_DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT .|.
          VK_DEBUG_REPORT_INFORMATION_BIT_EXT .|.
          VK_DEBUG_REPORT_DEBUG_BIT_EXT,
        drcci'callbackFunPtr = castFunPtr debugCallbackFunPtr,
        drcci'withUserDataPtr = return userDataPtr
      }
    )
    (return nullPtr)
    (return nullPtr)
  putStrLn "Debug report callback registered."


  debugReportMessageEXT debugReportExt
    VK_DEBUG_REPORT_DEBUG_BIT_EXT
    VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
    1
    2
    3
    (return layerNamePtr)
    (return messagePtr)
#endif

  {-
  lowerCodensity $ do
    (physDevicesArrayLen, numPhysDevices, physDevicesPtr) <-
      withEnumeratedPhysicalDevices vkInstance $ \count -> Codensity $ allocaArray (fromIntegral count)
    liftIO $ do
      putStrLn $ "Phys devices array length: " ++ show physDevicesArrayLen
      putStrLn $ "Number of phys devices: " ++ show numPhysDevices
  -}

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
  VK_EXT_DEBUG_REPORT_EXTENSION_NAME :
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
