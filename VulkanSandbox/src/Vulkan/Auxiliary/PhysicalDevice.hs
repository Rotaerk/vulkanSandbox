module Vulkan.Auxiliary.PhysicalDevice (
  withEnumeratedPhysicalDevices
) where

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Word
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Vulkan.Auxiliary.Core
import Vulkan.Auxiliary.Exception

withEnumeratedPhysicalDevices ::
  VkInstance ->
  (Word32 -> Codensity IO (Ptr VkPhysicalDevice)) ->
  Codensity IO (Word32, Word32, Ptr VkPhysicalDevice)
withEnumeratedPhysicalDevices vkInstance withPhysDeviceArrayOfLength = do
  physDevicesCountPtr <- Codensity alloca
  void $ liftIO $ vkEnumeratePhysicalDevices vkInstance physDevicesCountPtr nullPtr >>=
    throwIfVkResultNotSuccess vkFunEnumeratePhysicalDevices
  physDevicesArrayLen <- liftIO $ peek physDevicesCountPtr
  if physDevicesArrayLen == 0 then
    return (0, 0, nullPtr)
  else do
    physDevicesPtr <- withPhysDeviceArrayOfLength physDevicesArrayLen
    void $ liftIO $ vkEnumeratePhysicalDevices vkInstance physDevicesCountPtr physDevicesPtr >>=
      throwIfVkResultNotIn [VK_SUCCESS, VK_INCOMPLETE] vkFunEnumeratePhysicalDevices
    physDevicesCount <- liftIO $ peek physDevicesCountPtr
    return (physDevicesArrayLen, physDevicesCount, physDevicesPtr)
