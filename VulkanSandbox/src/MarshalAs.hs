{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}

module MarshalAs (
  MarshalAs(..),
  allocaMarshal
) where

import Local.Foreign.Marshal.Alloc
import Local.Control.Monad.Codensity

import Control.Monad.IO.Class
import Foreign.Ptr
import Foreign.Storable

class Storable a => MarshalAs a b | b -> a where
  marshalTo :: Ptr a -> b -> IO ()

allocaMarshal ::
  forall a b.
  MarshalAs a b =>
  b -> SomeIOWith (Ptr a)
allocaMarshal b = runCodensity do
  ptr <- Codensity alloca
  liftIO $ marshalTo ptr b
  return ptr
