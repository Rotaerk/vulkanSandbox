{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary (
  module Vulkan.Auxiliary.DefaultSType,
  module Vulkan.Auxiliary.Exception,
  module Vulkan.Core_1_0,
  vkaSetPtrNextToNull,
) where

import Local.Foreign.Storable.Offset

import Foreign.Ptr
import GHC.Records
import Vulkan.Auxiliary.DefaultSType
import Vulkan.Auxiliary.Exception
import Vulkan.Core_1_0

vkaSetPtrNextToNull ::
  forall r.
  (
    HasField "pNext" r (Ptr ()),
    Offset "pNext" r
  ) =>
  (?ptr :: Ptr r) =>
  IO ()
vkaSetPtrNextToNull = pokePtrOffset @"pNext" nullPtr
