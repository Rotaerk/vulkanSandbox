{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Local.Prelude

import Local.Foreign.Marshal.Alloc
import Local.Foreign.Storable.Offset
import qualified Local.Graphics.UI.GLFW as GLFW

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
import Foreign.Storable
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
mainBody = withNewScope_ do
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

  instanceExtensions <- (appInstanceExtensions ++) <$> liftIO GLFW.getRequiredInstanceExtensions
  putStrLn "Identified required vulkan extensions."

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

{-
resourceMain :: ResourceT IO ()
resourceMain = do
  liftIO $ GLFW.setErrorCallback . Just $ \errorCode errorMessage ->
    GLFW.throwGLFWExceptionM ("GLFW error callback: " ++ show errorCode ++ " - " ++ errorMessage)
  ioPutStrLn "GLFW error callback set."

  allocateAcquire_ GLFW.acquireInitialization
  ioPutStrLn "GLFW initialized."

  liftIO $ GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)

  window <- allocateAcquire_ $ GLFW.acquireWindow 800 600 "Vulkan Sandbox"
  ioPutStrLn "GLFW window created."

  lastWindowResizeTimeRef <- liftIO $ newIORef Nothing
  liftIO $ GLFW.setFramebufferSizeCallback window $ Just $ \_ _ _ -> do
    time <- getTime Monotonic
    writeIORef lastWindowResizeTimeRef $ Just time
  ioPutStrLn "Window framebuffer size callback registered."

  glfwInstanceExtensions <- liftIO GLFW.getRequiredInstanceExtensions

  let requiredInstanceExtensions = glfwInstanceExtensions ++ appInstanceExtensions

  vulkanInstance <- liftIO $
    alloca @VkInstanceCreateInfo $ \createInfoPtr ->
    alloca @VkApplicationInfo $ \appInfoPtr ->
    withCString utf8 "Vulkan Sandbox" $ \appNamePtr ->
    withArray requiredInstanceExtensions $ \extsPtr ->
    withCStringsLen utf8 validationLayers $ \numLayers layersPtr -> do
      let ?ptr = appInfoPtr in do
        vkaSetPtrSTypeToDefault
        vkaSetPtrNextToNull
        setPtrField @"pApplicationName" (castPtr appNamePtr)
        setPtrField @"applicationVersion" (VK_MAKE_VERSION 1 0 0)
        setPtrField @"pEngineName" nullPtr
        setPtrField @"engineVersion" 0
        setPtrField @"apiVersion" VK_API_VERSION_1_0
      let ?ptr = createInfoPtr in do
        vkaSetPtrSTypeToDefault
        vkaSetPtrNextToNull
        setPtrField @"flags" 0
        setPtrField @"pApplicationInfo" appInfoPtr
        setPtrField @"enabledLayerCount" (fromIntegral numLayers)
        setPtrField @"ppEnabledLayerNames" (castPtr layersPtr)
        setPtrField @"enabledExtensionCount" (fromIntegral $ length requiredInstanceExtensions)
        setPtrField @"ppEnabledExtensionNames" (castPtr extsPtr)
      alloca $ \instancePtr -> do
        vkCreateInstance createInfoPtr nullPtr instancePtr >>=
          vkaThrowIfResultNotSuccess "vkCreateInstance"
        peek instancePtr
  ioPutStrLn "Vulkan instance created."

  ioPutStrLn "Render loop starting."
  liftIO $ fix $ \renderLoop ->
    GLFW.getWindowStatus window lastWindowResizeTimeRef >>= \case
      GLFW.WindowResized -> do
        (width, height) <- GLFW.getFramebufferSize window
        renderLoop
      GLFW.WindowClosed -> pure ()
      GLFW.WindowReady -> do
        whenM (GLFW.getKey window GLFW.Key'Escape <&> (== GLFW.KeyState'Pressed)) $
          GLFW.setWindowShouldClose window True
        renderLoop
-}

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
