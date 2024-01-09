module Vulkan.Auxiliary.Ext.VK_EXT_debug_utils (
  module Vulkan.Ext.VK_EXT_debug_utils,
  VkDebugUtilsMessengerCreateInfoEXTFields(..)
) where

import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Foreign.C.String
import Foreign.Storable
import MarshalAs
import ScopedResource
import Vulkan.Auxiliary.Core
import Vulkan.Auxiliary.Exception
import Vulkan.Auxiliary.Instance
import Vulkan.Ext.VK_EXT_debug_utils

data VkDebugUtilsLabelEXTFields =
  VkDebugUtilsLabelEXTFields {
    withLabelNamePtr :: Codensity IO CString
  }

instance MarshalAs VkDebugUtilsLabelEXT VkDebugUtilsLabelEXTFields where
  marshalTo ptr fields = lowerCodensity do
    labelNamePtr <- fields.withLabelNamePtr
    liftIO $ runWithPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
      pokePtrOffset @"pNext" nullPtr
      pokePtrOffset @"pLabelName" (castPtr labelNamePtr)

data VkDebugUtilsMessengerCreateInfoEXTFields =
  VkDebugUtilsMessengerCreateInfoEXTFields {
    withNextPtr :: Codensity IO (Ptr ()),
    messageSeverity :: VkDebugUtilsMessageSeverityFlagBitsEXT,
    messageType :: VkDebugUtilsMessageTypeFlagBitsEXT,
    userCallbackFunPtr :: FunPtr PFN_vkDebugUtilsMessengerCallbackEXT,
    withUserDataPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXTFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    userDataPtr <- fields.withUserDataPtr
    liftIO $ runWithPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" 0
      pokePtrOffset @"messageSeverity" fields.messageSeverity
      pokePtrOffset @"messageType" fields.messageType
      pokePtrOffset @"pfnUserCallback" fields.userCallbackFunPtr
      pokePtrOffset @"pUserData" userDataPtr
