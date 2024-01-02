{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

module Scope (
  Scope(),
  ImplicitScope,
  theScope,
  withImplicitScope,
  withNewImplicitScope,
  withNewImplicitScope_,
  onScopeExit,
  scoped
) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Scope s = Scope { scope'onExitRef :: IORef (IO ()) }

type ImplicitScope s = (?scope :: Scope s)

theScope :: ImplicitScope s => Scope s
theScope = ?scope

withImplicitScope :: Scope s -> (ImplicitScope s => a) -> a
withImplicitScope scope body = let ?scope = scope in body

withNewImplicitScope :: (forall s. ImplicitScope s => Scope s -> IO a) -> IO a
withNewImplicitScope scopedAction = withNewImplicitScope_ (scopedAction theScope)

withNewImplicitScope_ :: (forall s. ImplicitScope s => IO a) -> IO a
withNewImplicitScope_ scopedAction = do
  onExitRef <- newIORef (pure ())
  let scope = Scope onExitRef
  withImplicitScope scope scopedAction `finally` join (readIORef onExitRef)

onScopeExit :: ImplicitScope s => IO () -> IO ()
onScopeExit action = modifyIORef (scope'onExitRef theScope) (action >>)

scoped :: ImplicitScope s => IO r -> (r -> IO ()) -> IO r
scoped acquire release = mask_ do
  resource <- acquire
  onScopeExit $ release resource
  return resource
