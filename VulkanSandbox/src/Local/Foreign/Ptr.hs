module Local.Foreign.Ptr (
  module Foreign.Ptr,
  DynamicImport,
  DynamicWrapper,
  haskellFunPtrResource
) where

import Foreign.Ptr
import ScopedResource

-- See https://www.haskell.org/onlinereport/haskell2010/haskellch8.html
type DynamicImport f = FunPtr f -> f
type DynamicWrapper f = f -> IO (FunPtr f)

haskellFunPtrResource :: DynamicWrapper f -> f -> Resource (FunPtr f)
haskellFunPtrResource wrap f = Resource (wrap f) freeHaskellFunPtr
{-# INLINE haskellFunPtrResource #-}
