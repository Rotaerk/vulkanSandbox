{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MarshalAs (
  MarshalAs(..),
  allocaMarshal
) where

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

class Storable a => MarshalAs a b | b -> a where
  marshalTo :: Ptr a -> b -> IO ()

allocaMarshal ::
  forall a b.
  MarshalAs a b =>
  b -> Codensity IO (Ptr a)
allocaMarshal b = do
  structPtr <- Codensity $ alloca @a
  liftIO $ marshalTo structPtr b
  return structPtr
