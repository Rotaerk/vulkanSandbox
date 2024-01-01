{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}

module Vulkan.Auxiliary.DefaultSType (
  VkaDefaultSType,
  vkaSetPtrSTypeToDefault
) where

import Local.Foreign.Storable.Offset

import Foreign.Ptr
import GHC.Records
import Vulkan.Core_1_0

class VkaDefaultSType r where
  vkaDefaultSType :: VkStructureType

vkaSetPtrSTypeToDefault ::
  forall r.
  (
    HasField "sType" r VkStructureType,
    Offset "sType" r,
    VkaDefaultSType r,
    ?ptr :: Ptr r
  ) =>
  IO ()
vkaSetPtrSTypeToDefault = pokePtrOffset @"sType" (vkaDefaultSType @r)

instance VkaDefaultSType VkInstanceCreateInfo where
  vkaDefaultSType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO

instance VkaDefaultSType VkApplicationInfo where
  vkaDefaultSType = VK_STRUCTURE_TYPE_APPLICATION_INFO
