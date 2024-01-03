{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

module ScopedResource (
  Scope(),
  ImplicitScope,
  theScope,
  withNewScope,
  withImplicitScope,
  withNewImplicitScope_,
  withNewImplicitScope,
  onExiting,
  onExitingThisScope,
  Resource(..),
  acquireIn,
  acquireInThisScope
) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Scope s = Scope (IORef (IO ()))

type ImplicitScope s = (?scope :: Scope s)

theScope :: ImplicitScope s => Scope s
theScope = ?scope

withNewScope :: (forall s. Scope s -> IO a) -> IO a
withNewScope scopedAction = do
  onExitRef <- newIORef (pure ())
  scopedAction (Scope onExitRef) `finally` join (readIORef onExitRef)

withImplicitScope :: Scope s -> (ImplicitScope s => IO a) -> IO a
withImplicitScope scope scopedAction = let ?scope = scope in scopedAction

withNewImplicitScope_ :: (forall s. ImplicitScope s => IO a) -> IO a
withNewImplicitScope_ scopedAction = withNewScope \scope -> withImplicitScope scope scopedAction

withNewImplicitScope :: (forall s. ImplicitScope s => Scope s -> IO a) -> IO a
withNewImplicitScope scopedAction = withNewImplicitScope_ (scopedAction ?scope)

onExiting :: Scope s -> IO () -> IO ()
onExiting (Scope onExitRef) action = modifyIORef onExitRef (action >>)

onExitingThisScope :: ImplicitScope s => IO () -> IO ()
onExitingThisScope = onExiting ?scope

data Resource r =
  Resource {
    resource'acquire :: IO r,
    resource'release :: r -> IO ()
  }

acquireIn :: Scope s -> Resource r -> IO r
acquireIn scope (Resource acquire release) = mask_ do
  resource <- acquire
  onExiting scope $ release resource
  return resource

acquireInThisScope :: ImplicitScope s => Resource r -> IO r
acquireInThisScope = acquireIn ?scope
