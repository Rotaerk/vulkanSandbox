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

import Local.Control.Monad.Codensity
import Local.Foreign.Marshal.Alloc
import Local.Foreign.Ptr

import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Storable.Offset
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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
      poke (offset @"pNext" ptr) nextPtr
      poke (offset @"flags" ptr) fields.flags
      poke (offset @"pfnCallback" ptr) fields.callbackFunPtr
      poke (offset @"pUserData" ptr) userDataPtr

createDebugReportCallback ::
  VK_EXT_debug_report ->
  VkInstance ->
  SomeIOWith (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  IO VkDebugReportCallbackEXT
createDebugReportCallback ext vkInstance withCreateInfoPtr withAllocatorPtr =
  withCreateInfoPtr \createInfoPtr ->
  withAllocatorPtr \allocatorPtr ->
  allocaPeek \ptr ->
    vkCreateDebugReportCallbackEXT ext vkInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateDebugReportCallbackEXT

debugReportMessage ::
  VK_EXT_debug_report ->
  VkInstance ->
  VkDebugReportFlagsEXT ->
  VkDebugReportObjectTypeEXT ->
  Word64 ->
  CSize ->
  Int32 ->
  SomeIOWith CString ->
  SomeIOWith CString ->
  IO ()
debugReportMessage ext vkInstance flags objectType object (CSize location) messageCode withLayerPrefixPtr withMessagePtr =
  withLayerPrefixPtr \layerPrefixPtr ->
  withMessagePtr \messagePtr ->
    vkDebugReportMessageEXT ext vkInstance
      flags objectType object location messageCode (castPtr layerPrefixPtr) (castPtr messagePtr)

destroyDebugReportCallback ::
  VK_EXT_debug_report ->
  VkInstance ->
  VkDebugReportCallbackEXT ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  IO ()
destroyDebugReportCallback ext vkInstance callback withAllocatorPtr =
  withAllocatorPtr $ vkDestroyDebugReportCallbackEXT ext vkInstance callback

debugReportCallbackResource ::
  VK_EXT_debug_report ->
  VkInstance ->
  SomeIOWith (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  Resource VkDebugReportCallbackEXT
debugReportCallbackResource ext vkInstance withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createDebugReportCallback ext vkInstance withCreateInfoPtr withCreateAllocatorPtr)
  (\callback -> destroyDebugReportCallback ext vkInstance callback withDestroyAllocatorPtr)
