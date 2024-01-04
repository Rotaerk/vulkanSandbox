{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImplicitParams #-}

module ScopedResource (
  Scope(),
  withNewScope,
  onExiting,
  Resource(..),
  acquireIn
) where

import Control.Exception
import Control.Monad
import Data.IORef

newtype Scope s = Scope (IORef (IO ()))

withNewScope :: (forall s. Scope s -> IO a) -> IO a
withNewScope scopedAction = do
  onExitRef <- newIORef (pure ())
  scopedAction (Scope onExitRef) `finally` join (readIORef onExitRef)

onExiting :: Scope s -> IO () -> IO ()
onExiting (Scope onExitRef) action = modifyIORef onExitRef (action >>)

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
