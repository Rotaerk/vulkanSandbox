{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Ext.VK_EXT_debug_report (
  module Vulkan.Ext.VK_EXT_debug_report,
  VkDebugReportCallbackCreateInfoEXTFields(..),
  createVkDebugReportCallbackEXT,
  debugReportMessageEXT,
  destroyVkDebugReportCallbackEXT,
  vkDebugReportCallbackEXTResource,
  VK_EXT_debug_report(..),
  wrapPFN_vkDebugReportCallbackEXT
) where

import Control.Monad.Codensity
import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import MarshalAs
import ScopedResource
import Vulkan.Auxiliary
import Vulkan.Ext.VK_EXT_debug_report

data VkDebugReportCallbackCreateInfoEXTFields =
  VkDebugReportCallbackCreateInfoEXTFields {
    withNextPtr :: Codensity IO (Ptr ()),
    flags :: VkDebugReportFlagsEXT,
    callbackFunPtr :: FunPtr PFN_vkDebugReportCallbackEXT,
    withUserDataPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXTFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    userDataPtr <- fields.withUserDataPtr
    liftIO $ withImplicitPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" fields.flags
      pokePtrOffset @"pfnCallback" fields.callbackFunPtr
      pokePtrOffset @"pUserData" userDataPtr

createVkDebugReportCallbackEXT ::
  VK_EXT_debug_report ->
  Codensity IO (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO VkDebugReportCallbackEXT
createVkDebugReportCallbackEXT ext withCreateInfoPtr withAllocatorPtr = lowerCodensity do
  createInfoPtr <- withCreateInfoPtr
  allocatorPtr <- withAllocatorPtr
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
  Codensity IO CString ->
  Codensity IO CString ->
  IO ()
debugReportMessageEXT ext flags objectType object location messageCode withLayerPrefixPtr withMessagePtr = lowerCodensity do
  layerPrefixPtr <- withLayerPrefixPtr
  messagePtr <- withMessagePtr
  liftIO $ vkDebugReportMessageEXT ext
    flags objectType object location messageCode (castPtr layerPrefixPtr) (castPtr messagePtr)

destroyVkDebugReportCallbackEXT ::
  VK_EXT_debug_report ->
  VkDebugReportCallbackEXT ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO ()
destroyVkDebugReportCallbackEXT ext callback withAllocatorPtr = lowerCodensity do
  allocatorPtr <- withAllocatorPtr
  liftIO $ vkDestroyDebugReportCallbackEXT ext callback allocatorPtr

vkDebugReportCallbackEXTResource ::
  VK_EXT_debug_report ->
  Codensity IO (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
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

foreign import ccall "wrapper" wrapPFN_vkDebugReportCallbackEXT :: DynamicWrapper PFN_vkDebugReportCallbackEXT
