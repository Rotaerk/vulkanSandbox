{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

module Scope (
  Scope(),
  Scoped,
  withScope,
  withNewScope,
  withNewScope_,
  onScopeExit,
  scoped
) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Scope s = Scope { scope'onExitRef :: IORef (IO ()) }

type Scoped s = (?scope :: Scope s)

withScope :: Scope s -> (Scoped s => IO a) -> IO a
withScope scope scopedAction = let ?scope = scope in scopedAction

withNewScope :: (forall s. Scoped s => Scope s -> IO a) -> IO a
withNewScope scopedAction = do
  onExitRef <- newIORef (pure ())
  let scope = Scope onExitRef
  withScope scope (scopedAction scope) `finally` join (readIORef onExitRef)

withNewScope_ :: (forall s. Scoped s => IO a) -> IO a
withNewScope_ scopedAction = withNewScope (const scopedAction)

onScopeExit :: Scoped s => IO () -> IO ()
onScopeExit action = modifyIORef (scope'onExitRef ?scope) (action >>)

scoped :: Scoped s => IO r -> (r -> IO ()) -> IO r
scoped acquire release = mask_ do
  resource <- acquire
  onScopeExit $ release resource
  return resource
