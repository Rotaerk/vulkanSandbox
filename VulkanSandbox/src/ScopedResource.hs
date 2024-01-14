{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module ScopedResource (
  Scope(),
  withNewScope,
  onExiting,
  ExitActionHandle(),
  tentativelyOnExiting,
  rescind,
  rescopeTo,
  tentativelyRescopeTo,
  Resource(..),
  acquireIn,
  tentativelyAcquireIn
) where

import Control.Exception
import Control.Monad
import Control.Monad.Extra
import Data.IORef

newtype Scope s = Scope { exitActionsRef :: IORef (IO ()) }

-- |Runs a continuation that can register actions with the provided scope,
-- and then runs all the registered actions afterwards, even if the
-- continuation throws an exception.
withNewScope :: (forall s. Scope s -> IO a) -> IO a
withNewScope scopedAction = do
  exitActionsRef <- newIORef (pure ())
  scopedAction (Scope exitActionsRef) `finally` join (readIORef exitActionsRef)

onExiting :: Scope s -> IO () -> IO ()
onExiting scope exitAction = modifyIORef scope.exitActionsRef (exitAction >>)

data ExitActionHandle s =
  ExitActionHandle {
    isRescindedRef :: IORef Bool,
    exitAction :: IO ()
  }

tentativelyOnExiting :: Scope s -> IO () -> IO (ExitActionHandle s)
tentativelyOnExiting scope exitAction = do
  isRescindedRef <- newIORef False
  onExiting scope $ unlessM (readIORef isRescindedRef) exitAction
  return $ ExitActionHandle isRescindedRef exitAction

rescind :: ExitActionHandle s -> IO ()
rescind eah = atomicModifyIORef' eah.isRescindedRef \case
  False -> (True, ())
  True -> error "Exit-action is already rescinded."

rescopeTo :: Scope s' -> ExitActionHandle s -> IO ()
rescopeTo scope' eah = mask_ do
  rescind eah
  onExiting scope' eah.exitAction

tentativelyRescopeTo :: Scope s' -> ExitActionHandle s -> IO (ExitActionHandle s')
tentativelyRescopeTo scope' eah = mask_ do
  rescind eah
  tentativelyOnExiting scope' eah.exitAction

data Resource a =
  Resource {
    acquire :: IO a,
    release :: a -> IO ()
  }

acquireIn :: Scope s -> Resource a -> IO a
acquireIn scope resource = mask_ do
  a <- resource.acquire
  onExiting scope $ resource.release a
  pure a

tentativelyAcquireIn :: Scope s -> Resource a -> IO (a, ExitActionHandle s)
tentativelyAcquireIn scope resource = mask_ do
  a <- resource.acquire
  eah <- tentativelyOnExiting scope $ resource.release a
  pure (a, eah)
