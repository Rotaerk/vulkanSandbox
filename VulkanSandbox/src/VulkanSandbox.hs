{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Local.Graphics.UI.GLFW as GLFW
import Local.Control.Monad.Cont

import ApplicationException
import Control.Exception
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Function
import Data.Functor
import Data.IORef
import Foreign.C.String hiding (withCString)
import Foreign.Marshal.Array
import Foreign.Ptr
import GHC.Foreign
import GHC.Ptr
import ScopedResource
import System.Clock
import System.IO
import Vulkan.Auxiliary
import Vulkan.Auxiliary.Ext.VK_EXT_debug_report

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
mainBody = withNewImplicitScope \mainScope -> do
  GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
    GLFW.throwGLFWExceptionM ("GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage)
  putStrLn "GLFW error callback set."

  acquireInThisScope GLFW.initializationResource
  putStrLn "GLFW initialized"

  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  window <- acquireInThisScope $ GLFW.windowResource 800 600 "Vulkan Sandbox" Nothing Nothing
  putStrLn "GLFW window created."

  lastWindowResizeTimeRef <- newIORef Nothing
  GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
    time <- getTime Monotonic
    writeIORef lastWindowResizeTimeRef $ Just time
  putStrLn "Window framebuffer size callback registered."

  requiredInstanceExtensions <- (appInstanceExtensions ++) <$> liftIO GLFW.getRequiredInstanceExtensions
  putStrLn "Identified required vulkan extensions."

  vulkanInstance <- acquireInThisScope $ vkaInstanceResource
    (
      vkaWithInstanceCreateInfoPtr VkaInstanceCreateInfo {
        withNextPtr = ($ nullPtr),
        flags = 0,

        withAppInfoPtr =
          vkaWithApplicationInfoPtr VkaApplicationInfo {
            withAppNamePtr = ($ appNamePtr),
            appVersion = VK_MAKE_VERSION 1 0 0,
            withEngineNamePtr = ($ nullPtr),
            engineVersion = 0,
            apiVersion = VK_API_VERSION_1_0
          },

        withEnabledLayerNamesPtrLen = runContT do
          (numLayers, layersPtr) <- ContT $ uncurryCPS2 (withArrayLen validationLayers)
          return (layersPtr, fromIntegral numLayers),

        withEnabledExtensionNamesPtrLen = runContT do
          (numExtensions, extensionsPtr) <- ContT $ uncurryCPS2 (withArrayLen requiredInstanceExtensions)
          return (extensionsPtr, fromIntegral numExtensions)
      }
    )
    ($ nullPtr)
    ($ nullPtr)
  putStrLn "Vulkan instance created."

  let ?vkInstance = vulkanInstance

#ifndef ndebug
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

appNamePtr :: CString
appNamePtr = Ptr "Vulkan Sandbox\0"#

appInstanceExtensions :: [CString]
appInstanceExtensions =
#ifndef ndebug
  VK_EXT_DEBUG_REPORT_EXTENSION_NAME :
#endif
  [
  ]

validationLayers :: [CString]
validationLayers =
#ifndef ndebug
  Ptr "VK_LAYER_KHRONOS_validation\0"# :
#endif
  [
  ]
