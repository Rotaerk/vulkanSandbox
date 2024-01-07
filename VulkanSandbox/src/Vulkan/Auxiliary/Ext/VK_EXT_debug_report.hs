{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vulkan.Auxiliary.Ext.VK_EXT_debug_report (
  module Vulkan.Ext.VK_EXT_debug_report,
  VkDebugReportCallbackCreateInfoEXTFields(..),
  createVkDebugReportCallbackEXT,
  debugReportMessageEXT,
  destroyVkDebugReportCallbackEXT,
  vkDebugReportCallbackEXTResource,
  VK_EXT_debug_report(..),
  MyPFN_vkDebugReportCallbackEXT,
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
    drcci'withNextPtr :: Codensity IO (Ptr ()),
    drcci'flags :: VkDebugReportFlagsEXT,
    drcci'callbackFunPtr :: FunPtr PFN_vkDebugReportCallbackEXT,
    drcci'withUserDataPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXTFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- drcci'withNextPtr fields
    userDataPtr <- drcci'withUserDataPtr fields
    liftIO $ withImplicitPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" (drcci'flags fields)
      pokePtrOffset @"pfnCallback" (drcci'callbackFunPtr fields)
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

type MyPFN_vkDebugReportCallbackEXT =
  VkDebugReportFlagsEXT
  -> VkDebugReportObjectTypeEXT
  -> Word64
  -> Word64
  -> Int32
  -> Ptr Int8
--  -> Word64
  -> Ptr Int8
  -> Ptr ()
  -> IO VkBool32

foreign import ccall "wrapper" wrapPFN_vkDebugReportCallbackEXT :: DynamicWrapper MyPFN_vkDebugReportCallbackEXT
