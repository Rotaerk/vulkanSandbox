{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}

module Local.Foreign.Storable.Offset (
  module Foreign.Storable.Offset,
  setPtrField
) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Offset
import GHC.Records

setPtrField ::
  forall x r a.
  (HasField x r a, Offset x r, Storable a) =>
  (?ptr :: Ptr r) =>
  a -> IO ()
setPtrField = poke (offset @x ?ptr)
{-# INLINE setPtrField #-}
