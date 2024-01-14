{-# LANGUAGE BlockArguments #-}

module Local.Foreign.Marshal.Alloc (
  module Foreign.Marshal.Alloc,
  mallocResource,
  allocaPeek
) where

import Control.Monad.Codensity
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import ScopedResource

mallocResource :: Storable a => Resource (Ptr a)
mallocResource = Resource malloc free
{-# INLINE mallocResource #-}

allocaPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaPeek usePtr = alloca \ptr -> usePtr ptr >> peek ptr
