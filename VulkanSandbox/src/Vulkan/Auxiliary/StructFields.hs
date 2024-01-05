{-# LANGUAGE FunctionalDependencies #-}

module Vulkan.Auxiliary.StructFields (
  VkStructFields(..)
) where

import Local.Control.Monad.Cont

import Foreign.Ptr

class VkStructFields fields vk | fields -> vk where
  withVkStructPtr :: fields r -> IOWith (Ptr vk) r
