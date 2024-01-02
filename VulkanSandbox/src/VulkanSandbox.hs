{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
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
import Scope
import System.Clock
import System.IO

import Vulkan.Auxiliary
import Vulkan.Ext.VK_EXT_debug_report

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
mainBody = withNewImplicitScope_ do
  GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
    GLFW.throwGLFWExceptionM ("GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage)
  putStrLn "GLFW error callback set."

  scoped GLFW.initOrThrow (const $ GLFW.terminate)
  putStrLn "GLFW initialized"

  GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
  window <- scoped (GLFW.createWindowOrThrow 800 600 "Vulkan Sandbox" Nothing Nothing) GLFW.destroyWindow
  putStrLn "GLFW window created."

  lastWindowResizeTimeRef <- newIORef Nothing
  GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
    time <- getTime Monotonic
    writeIORef lastWindowResizeTimeRef $ Just time
  putStrLn "Window framebuffer size callback registered."

  requiredInstanceExtensions <- (appInstanceExtensions ++) <$> liftIO GLFW.getRequiredInstanceExtensions
  putStrLn "Identified required vulkan extensions."

  vulkanInstance <- vkaCreateScopedInstance
    (
      vkaWithInstanceCreateInfoPtr VkaInstanceCreateInfo {
        withNextPtr = ($ nullPtr),
        flags = 0,

        withAppInfoPtr =
          vkaWithApplicationInfoPtr VkaApplicationInfo {
            withAppNamePtr = withCString utf8 "Vulkan Sandbox",
            appVersion = VK_MAKE_VERSION 1 0 0,
            withEngineNamePtr = ($ nullPtr),
            engineVersion = 0,
            apiVersion = VK_API_VERSION_1_0
          },

        withEnabledLayerNamesPtrLen = runContT do
          (numLayers, layersPtr) <- ContT $ uncurryWith2 (withCStringsLen utf8 validationLayers)
          return (layersPtr, fromIntegral numLayers),

        withEnabledExtensionNamesPtrLen = runContT do
          (numExtensions, extensionsPtr) <- ContT $ uncurryWith2 (withArrayLen requiredInstanceExtensions)
          return (extensionsPtr, fromIntegral numExtensions)
      }
    )
    ($ nullPtr)
    ($ nullPtr)
  putStrLn "Vulkan instance created."

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
  [
  ]
#ifndef NDEBUG
  ++
  [
    VK_EXT_DEBUG_REPORT_EXTENSION_NAME
  ]
#endif

validationLayers :: [String]
validationLayers =
  [
#ifndef NDEBUG
    "VK_LAYER_KHRONOS_validation"
#endif
  ]
