{-# LANGUAGE BlockArguments #-}

module Local.Foreign.Marshal.Alloc (
  module Foreign.Marshal.Alloc,
  allocaPeek,
  scopedMalloc
) where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Scope

{-# INLINE allocaPeek #-}
allocaPeek :: Storable a => (Ptr a -> IO ()) -> IO a
allocaPeek f = alloca \ptr -> f ptr >> peek ptr

{-# INLINE scopedMalloc #-}
scopedMalloc :: (Storable a, Scoped s) => IO (Ptr a)
scopedMalloc = scoped malloc free
