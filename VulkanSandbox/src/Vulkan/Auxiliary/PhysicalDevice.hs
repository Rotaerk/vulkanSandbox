module Vulkan.Auxiliary.PhysicalDevice (
  withEnumeratedPhysicalDevices,
  withPhysicalDeviceProperties,
  withPhysicalDeviceMemoryProperties,
  withPhysicalDeviceFeatures
) where

import Local.Control.Monad.Codensity

import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Vulkan.Auxiliary.Core
import Vulkan.Auxiliary.Exception

withEnumeratedPhysicalDevices ::
  VkInstance ->
  (forall r. Word32 -> IOWith (Ptr VkPhysicalDevice) r) ->
  SomeIOWith (Word32, Ptr VkPhysicalDevice)
withEnumeratedPhysicalDevices vkInstance withPhysDeviceArrayOfLength = runCodensity do
  physDevicesCountPtr <- Codensity alloca
  physDevicesArrayLen <- liftIO do
    void $ vkEnumeratePhysicalDevices vkInstance physDevicesCountPtr nullPtr >>=
      throwIfVkResultNotSuccess vkFunEnumeratePhysicalDevices
    peek physDevicesCountPtr
  if physDevicesArrayLen == 0 then
    return (0, nullPtr)
  else do
    physDevicesPtr <- Codensity $ withPhysDeviceArrayOfLength physDevicesArrayLen
    void $ liftIO $ vkEnumeratePhysicalDevices vkInstance physDevicesCountPtr physDevicesPtr >>=
      throwIfVkResultNotIn [VK_SUCCESS, VK_INCOMPLETE] vkFunEnumeratePhysicalDevices
    physDevicesCount <- liftIO $ peek physDevicesCountPtr
    return (physDevicesCount, physDevicesPtr)

withPhysicalDeviceProperties ::
  VkPhysicalDevice ->
  SomeIOWith (Ptr VkPhysicalDeviceProperties)
withPhysicalDeviceProperties physDevice =
  beforeCont (vkGetPhysicalDeviceProperties physDevice) alloca

withPhysicalDeviceMemoryProperties ::
  VkPhysicalDevice ->
  SomeIOWith (Ptr VkPhysicalDeviceMemoryProperties)
withPhysicalDeviceMemoryProperties physDevice =
  beforeCont (vkGetPhysicalDeviceMemoryProperties physDevice) alloca

withPhysicalDeviceFeatures ::
  VkPhysicalDevice ->
  SomeIOWith (Ptr VkPhysicalDeviceFeatures)
withPhysicalDeviceFeatures physDevice =
  beforeCont (vkGetPhysicalDeviceFeatures physDevice) alloca

--getPhysicalDevice
