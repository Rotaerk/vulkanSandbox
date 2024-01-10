module Vulkan.Auxiliary.Ext.VK_EXT_debug_report (
  module Vulkan.Ext.VK_EXT_debug_report,
  VK_EXT_debug_report(..),
  wrapPFN_vkDebugReportCallbackEXT,
  DebugReportCallbackCreateInfo(..),
  createDebugReportCallback,
  debugReportMessage,
  destroyDebugReportCallback,
  debugReportCallbackResource,
) where

import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable
import MarshalAs
import ScopedResource
import Vulkan.Auxiliary.Core
import Vulkan.Auxiliary.Exception
import Vulkan.Auxiliary.Instance
import Vulkan.Ext.VK_EXT_debug_report

data VK_EXT_debug_report =
  VK_EXT_debug_report {
    vkCreateDebugReportCallbackEXT :: VkCreateDebugReportCallbackEXT,
    vkDebugReportMessageEXT :: VkDebugReportMessageEXT,
    vkDestroyDebugReportCallbackEXT :: VkDestroyDebugReportCallbackEXT
  }

instance InstanceExtension VK_EXT_debug_report where
  getInstanceExtension vkInstance = VK_EXT_debug_report <$>
    getInstanceFun vkInstance vkFunCreateDebugReportCallbackEXT importVkCreateDebugReportCallbackEXT <*>
    getInstanceFun vkInstance vkFunDebugReportMessageEXT importVkDebugReportMessageEXT <*>
    getInstanceFun vkInstance vkFunDestroyDebugReportCallbackEXT importVkDestroyDebugReportCallbackEXT

foreign import capi "dynamic" importVkCreateDebugReportCallbackEXT :: DynamicImport VkCreateDebugReportCallbackEXT
foreign import capi "dynamic" importVkDebugReportMessageEXT :: DynamicImport VkDebugReportMessageEXT
foreign import capi "dynamic" importVkDestroyDebugReportCallbackEXT :: DynamicImport VkDestroyDebugReportCallbackEXT

foreign import ccall "wrapper" wrapPFN_vkDebugReportCallbackEXT :: DynamicWrapper PFN_vkDebugReportCallbackEXT

data DebugReportCallbackCreateInfo =
  DebugReportCallbackCreateInfo {
    withNextPtr :: Codensity IO (Ptr ()),
    flags :: VkDebugReportFlagsEXT,
    callbackFunPtr :: FunPtr PFN_vkDebugReportCallbackEXT,
    withUserDataPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugReportCallbackCreateInfoEXT DebugReportCallbackCreateInfo where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    userDataPtr <- fields.withUserDataPtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" fields.flags
      pokePtrOffset @"pfnCallback" fields.callbackFunPtr
      pokePtrOffset @"pUserData" userDataPtr

createDebugReportCallback ::
  VK_EXT_debug_report ->
  VkInstance ->
  Codensity IO (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO VkDebugReportCallbackEXT
createDebugReportCallback ext vkInstance withCreateInfoPtr withAllocatorPtr = lowerCodensity do
  createInfoPtr <- withCreateInfoPtr
  allocatorPtr <- withAllocatorPtr
  ptr <- Codensity alloca
  liftIO do
    vkCreateDebugReportCallbackEXT ext vkInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateDebugReportCallbackEXT
    peek ptr

debugReportMessage ::
  VK_EXT_debug_report ->
  VkInstance ->
  VkDebugReportFlagsEXT ->
  VkDebugReportObjectTypeEXT ->
  Word64 ->
  CSize ->
  Int32 ->
  Codensity IO CString ->
  Codensity IO CString ->
  IO ()
debugReportMessage ext vkInstance flags objectType object (CSize location) messageCode withLayerPrefixPtr withMessagePtr = lowerCodensity do
  layerPrefixPtr <- withLayerPrefixPtr
  messagePtr <- withMessagePtr
  liftIO $ vkDebugReportMessageEXT ext vkInstance
    flags objectType object location messageCode (castPtr layerPrefixPtr) (castPtr messagePtr)

destroyDebugReportCallback ::
  VK_EXT_debug_report ->
  VkInstance ->
  VkDebugReportCallbackEXT ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO ()
destroyDebugReportCallback ext vkInstance callback withAllocatorPtr = lowerCodensity do
  allocatorPtr <- withAllocatorPtr
  liftIO $ vkDestroyDebugReportCallbackEXT ext vkInstance callback allocatorPtr

debugReportCallbackResource ::
  VK_EXT_debug_report ->
  VkInstance ->
  Codensity IO (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Resource VkDebugReportCallbackEXT
debugReportCallbackResource ext vkInstance withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createDebugReportCallback ext vkInstance withCreateInfoPtr withCreateAllocatorPtr)
  (\callback -> destroyDebugReportCallback ext vkInstance callback withDestroyAllocatorPtr)
