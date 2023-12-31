{-# LANGUAGE LambdaCase #-}

module Local.Graphics.UI.GLFW (
  module Graphics.UI.GLFW,
  GLFWException(..),
  throwGLFWExceptionM,
  initOrThrow,
  createWindowOrThrow,
  WindowStatus(..),
  getWindowStatus
) where

import Control.Exception
import Control.Monad.Catch
import Control.Monad.Extra
import Data.IORef
import Graphics.UI.GLFW
import qualified Graphics.UI.GLFW as GLFW
import System.Clock as Clock

data GLFWException = GLFWException { glfwException'functionName :: String } deriving (Eq, Show, Read)

instance Exception GLFWException where
  displayException (GLFWException functionName) = "GLFWException: " ++ functionName ++ " failed."

throwGLFWExceptionM :: MonadThrow m => String -> m a
throwGLFWExceptionM = throwM . GLFWException

initOrThrow :: IO ()
initOrThrow = unlessM GLFW.init $ throwGLFWExceptionM "init"

createWindowOrThrow :: Int -> Int -> String -> Maybe Monitor -> Maybe Window -> IO GLFW.Window
createWindowOrThrow width height title mmon mwin =
  fromMaybeM (throwGLFWExceptionM "createWindow") $
  GLFW.createWindow width height title mmon mwin

data WindowStatus = WindowReady | WindowResized | WindowClosed

getWindowStatus :: GLFW.Window -> IORef (Maybe TimeSpec) -> IO WindowStatus
getWindowStatus window lastResizeTimeRef =
  GLFW.windowShouldClose window >>= \case
    True -> return WindowClosed
    False -> do
      GLFW.pollEvents
      currentTime <- Clock.getTime Monotonic
      -- GLFW sends many resize events during the resizing process, and doesn't say when the user is done resizing.
      -- Thus, only consider it resized after some time has passed since the last event.
      atomicModifyIORef lastResizeTimeRef $ \case
        Just lastResizeTime | currentTime - lastResizeTime >= resizeDelay -> (Nothing, WindowResized)
        v -> (v, WindowReady)
  where
    resizeDelay = fromNanoSecs (100 * 1000 * 1000) -- 100 milliseconds
