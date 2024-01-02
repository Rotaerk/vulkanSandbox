{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImplicitParams #-}

module Local.Foreign.Storable.Offset (
  module Foreign.Storable.Offset,
  ImplicitPtr,
  thePtr,
  withImplicitPtr,
  pokePtrOffset,
) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Offset
import GHC.Records

type ImplicitPtr r = (?ptr :: Ptr r)

thePtr :: ImplicitPtr r => Ptr r
thePtr = ?ptr

withImplicitPtr :: Ptr r -> (ImplicitPtr r => a) -> a
withImplicitPtr ptr body = let ?ptr = ptr in body

pokePtrOffset ::
  forall x r a.
  (HasField x r a, Offset x r, Storable a, ImplicitPtr r) =>
  a -> IO ()
pokePtrOffset = poke (offset @x thePtr)
{-# INLINE pokePtrOffset #-}
