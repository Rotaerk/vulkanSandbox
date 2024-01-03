{-# LANGUAGE BlockArguments #-}

module Local.Foreign.Marshal.Alloc (
  module Foreign.Marshal.Alloc,
  mallocResource
) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import ScopedResource

mallocResource :: Storable a => Resource (Ptr a)
mallocResource = MkResource malloc free
{-# INLINE mallocResource #-}
