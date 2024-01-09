module Vulkan.Auxiliary.Ext.VK_EXT_debug_utils (
  module Vulkan.Ext.VK_EXT_debug_utils,
  VkDebugUtilsLabelEXTFields(..),
  VkDebugUtilsMessengerCallbackDataEXTFields(..),
  VkDebugUtilsObjectTagInfoEXTFields(..),
  VkDebugUtilsMessengerCreateInfoEXTFields(..),
  VkDebugUtilsObjectNameInfoEXTFields(..)
) where

import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import MarshalAs
import ScopedResource
import Vulkan.Auxiliary.Core
import Vulkan.Auxiliary.Exception
import Vulkan.Auxiliary.Instance
import Vulkan.Ext.VK_EXT_debug_utils

data VkDebugUtilsLabelEXTFields =
  VkDebugUtilsLabelEXTFields {
    withLabelNamePtr :: Codensity IO CString,
    color :: (Float, Float, Float, Float)
  }

instance MarshalAs VkDebugUtilsLabelEXT VkDebugUtilsLabelEXTFields where
  marshalTo ptr fields = lowerCodensity do
    labelNamePtr <- fields.withLabelNamePtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
      pokePtrOffset @"pNext" nullPtr
      pokePtrOffset @"pLabelName" (castPtr labelNamePtr)
      pokePtrArrayOffset @"color" (let (r,g,b,a) = fields.color in [r,g,b,a])

data VkDebugUtilsMessengerCallbackDataEXTFields =
  VkDebugUtilsMessengerCallbackDataEXTFields {
    withNextPtr :: Codensity IO (Ptr ()),
    withMessageIdNamePtr :: Codensity IO CString,
    messageIdNumber :: Int32,
    withMessagePtr :: Codensity IO CString,
    queueLabelCount :: Word32,
    withQueueLabelsPtr :: Codensity IO (Ptr VkDebugUtilsLabelEXT),
    cmdBufLabelCount :: Word32,
    withCmdBufLabelsPtr :: Codensity IO (Ptr VkDebugUtilsLabelEXT),
    objectCount :: Word32,
    withObjectsPtr :: Codensity IO (Ptr VkDebugUtilsObjectNameInfoEXT)
  }

instance MarshalAs VkDebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXTFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    messageIdNamePtr <- fields.withMessageIdNamePtr
    messagePtr <- fields.withMessagePtr
    queueLabelsPtr <- fields.withQueueLabelsPtr
    cmdBufLabelsPtr <- fields.withCmdBufLabelsPtr
    objectsPtr <- fields.withObjectsPtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" 0
      pokePtrOffset @"pMessageIdName" (castPtr messageIdNamePtr)
      pokePtrOffset @"messageIdNumber" fields.messageIdNumber
      pokePtrOffset @"pMessage" (castPtr messagePtr)
      pokePtrOffset @"queueLabelCount" fields.queueLabelCount
      pokePtrOffset @"pQueueLabels" queueLabelsPtr
      pokePtrOffset @"cmdBufLabelCount" fields.cmdBufLabelCount
      pokePtrOffset @"pCmdBufLabels" cmdBufLabelsPtr
      pokePtrOffset @"objectCount" fields.objectCount
      pokePtrOffset @"pObjects" objectsPtr

data VkDebugUtilsObjectTagInfoEXTFields =
  VkDebugUtilsObjectTagInfoEXTFields {
    objectType :: VkObjectType,
    objectHandle :: Word64,
    tagName :: Word64,
    tagSize :: CSize,
    withTagPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXTFields where
  marshalTo ptr fields = lowerCodensity do
    tagPtr <- fields.withTagPtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
      pokePtrOffset @"pNext" nullPtr
      pokePtrOffset @"objectType" fields.objectType
      pokePtrOffset @"objectHandle" fields.objectHandle
      pokePtrOffset @"tagName" fields.tagName
      pokePtrOffset @"tagSize" (let (CSize w) = fields.tagSize in w)
      pokePtrOffset @"pTag" tagPtr

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
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" 0
      pokePtrOffset @"messageSeverity" fields.messageSeverity
      pokePtrOffset @"messageType" fields.messageType
      pokePtrOffset @"pfnUserCallback" fields.userCallbackFunPtr
      pokePtrOffset @"pUserData" userDataPtr

data VkDebugUtilsObjectNameInfoEXTFields =
  VkDebugUtilsObjectNameInfoEXTFields {
    withNextPtr :: Codensity IO (Ptr ()),
    objectType :: VkObjectType,
    objectHandle :: Word64,
    withObjectNamePtr :: Codensity IO CString
  }

instance MarshalAs VkDebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXTFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    objectNamePtr <- fields.withObjectNamePtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"objectType" fields.objectType
      pokePtrOffset @"objectHandle" fields.objectHandle
      pokePtrOffset @"pObjectName" (castPtr objectNamePtr)

foreign import ccall "wrapper" wrapPFN_vkDebugUtilsMessengerCallbackEXT :: DynamicWrapper PFN_vkDebugUtilsMessengerCallbackEXT
