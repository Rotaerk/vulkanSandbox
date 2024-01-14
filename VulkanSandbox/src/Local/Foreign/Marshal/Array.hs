{-# LANGUAGE LambdaCase #-}

module Local.Foreign.Marshal.Array (
  module Foreign.Marshal.Array,
  withArrayLenTuple,
  arrayElemPtrs
) where

import Local.Control.Monad.Codensity

import Control.Monad
import Data.List
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

withArrayLenTuple :: Storable a => [a] -> IOWith (Int, Ptr a) r
withArrayLenTuple vals = withUncurriedCont (withArrayLen vals)
{-# INLINE withArrayLenTuple #-}

arrayElemPtrs :: Storable a => Ptr a -> Int -> [Ptr a]
arrayElemPtrs = curry $ unfoldr \(ptr, size) -> do
  guard $ size > 0
  pure (ptr, (advancePtr ptr 1, size - 1))
{-# INLINE arrayElemPtrs #-}
