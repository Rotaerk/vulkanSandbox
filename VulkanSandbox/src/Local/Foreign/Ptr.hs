module Local.Foreign.Ptr (
  module Foreign.Ptr,
  DynamicImport,
  DynamicWrapper
) where

import Foreign.Ptr

-- See https://www.haskell.org/onlinereport/haskell2010/haskellch8.html
type DynamicImport ft = FunPtr ft -> ft
type DynamicWrapper ft = ft -> IO (FunPtr ft)
