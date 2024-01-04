{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CApiFFI #-}

module Vulkan.Auxiliary.Ext.VK_EXT_debug_report (
  module Vulkan.Ext.VK_EXT_debug_report,
  createVkDebugReportCallbackEXT,
  debugReportMessageEXT,
  destroyVkDebugReportCallbackEXT,
  vkDebugReportCallbackEXTResource,
  VK_EXT_debug_report(..)
) where

import Local.Control.Monad.Cont
import Local.Foreign.Ptr

import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import ScopedResource
import Vulkan.Auxiliary
import Vulkan.Ext.VK_EXT_debug_report

createVkDebugReportCallbackEXT ::
  VK_EXT_debug_report ->
  SomeIOCPS (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  IO VkDebugReportCallbackEXT
createVkDebugReportCallbackEXT ext withCreateInfoPtr withAllocatorPtr = evalContT $ do
  createInfoPtr <- ContT withCreateInfoPtr
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkCreateDebugReportCallbackEXT ext createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateDebugReportCallbackEXT
    peek ptr

debugReportMessageEXT ::
  VK_EXT_debug_report ->
  VkDebugReportFlagsEXT ->
  VkDebugReportObjectTypeEXT ->
  Word64 ->
  Word64 ->
  Int32 ->
  SomeIOCPS CString ->
  SomeIOCPS CString ->
  IO ()
debugReportMessageEXT ext flags objectType object location messageCode withLayerPrefixPtr withMessagePtr = evalContT $ do
  layerPrefixPtr <- ContT withLayerPrefixPtr
  messagePtr <- ContT withMessagePtr
  liftIO $ vkDebugReportMessageEXT ext
    flags objectType object location messageCode (castPtr layerPrefixPtr) (castPtr messagePtr)

destroyVkDebugReportCallbackEXT ::
  VK_EXT_debug_report ->
  VkDebugReportCallbackEXT ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  IO ()
destroyVkDebugReportCallbackEXT ext callback withAllocatorPtr = evalContT $ do
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ vkDestroyDebugReportCallbackEXT ext callback allocatorPtr

vkDebugReportCallbackEXTResource ::
  VK_EXT_debug_report ->
  SomeIOCPS (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  Resource VkDebugReportCallbackEXT
vkDebugReportCallbackEXTResource ext withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createVkDebugReportCallbackEXT ext withCreateInfoPtr withCreateAllocatorPtr)
  (\callback -> destroyVkDebugReportCallbackEXT ext callback withDestroyAllocatorPtr)

data VK_EXT_debug_report =
  VK_EXT_debug_report {
    vkCreateDebugReportCallbackEXT ::
      Ptr VkDebugReportCallbackCreateInfoEXT ->
      Ptr VkAllocationCallbacks ->
      Ptr VkDebugReportCallbackEXT ->
      IO VkResult,
    vkDebugReportMessageEXT ::
      VkDebugReportFlagsEXT ->
      VkDebugReportObjectTypeEXT ->
      Word64 ->
      Word64 ->
      Int32 ->
      Ptr Int8 ->
      Ptr Int8 ->
      IO (),
    vkDestroyDebugReportCallbackEXT ::
      VkDebugReportCallbackEXT ->
      Ptr VkAllocationCallbacks ->
      IO ()
  }

instance VkInstanceExtension VK_EXT_debug_report where
  getVkInstanceExtension vkInstance = VK_EXT_debug_report <$>
    getVkInstanceFun vkInstance vkFunCreateDebugReportCallbackEXT importVkCreateDebugReportCallbackEXT <*>
    getVkInstanceFun vkInstance vkFunDebugReportMessageEXT importVkDebugReportMessageEXT <*>
    getVkInstanceFun vkInstance vkFunDestroyDebugReportCallbackEXT importVkDestroyDebugReportCallbackEXT

foreign import capi "dynamic" importVkCreateDebugReportCallbackEXT :: DynamicImport VkCreateDebugReportCallbackEXT
foreign import capi "dynamic" importVkDebugReportMessageEXT :: DynamicImport VkDebugReportMessageEXT
foreign import capi "dynamic" importVkDestroyDebugReportCallbackEXT :: DynamicImport VkDestroyDebugReportCallbackEXT
