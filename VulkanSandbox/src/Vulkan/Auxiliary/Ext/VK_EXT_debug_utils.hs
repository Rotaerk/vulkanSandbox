module Vulkan.Auxiliary.Ext.VK_EXT_debug_utils (
  module Vulkan.Ext.VK_EXT_debug_utils,
  VK_EXT_debug_utils(..),
  wrapPFN_vkDebugUtilsMessengerCallbackEXT,
  DebugUtilsLabel(..),
  DebugUtilsMessengerCallbackData(..),
  DebugUtilsObjectTagInfo(..),
  DebugUtilsMessengerCreateInfo(..),
  DebugUtilsObjectNameInfo(..),
  cmdBeginDebugUtilsLabel,
  cmdEndDebugUtilsLabel,
  cmdInsertDebugUtilsLabel,
  createDebugUtilsMessenger,
  destroyDebugUtilsMessenger,
  debugUtilsMessengerResource,
  queueBeginDebugUtilsLabel,
  queueEndDebugUtilsLabel,
  queueInsertDebugUtilsLabel,
  setDebugUtilsObjectName,
  setDebugUtilsObjectTag,
  submitDebugUtilsMessage
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
import Vulkan.Ext.VK_EXT_debug_utils

data VK_EXT_debug_utils =
  VK_EXT_debug_utils {
    vkCmdBeginDebugUtilsLabelEXT :: VkCmdBeginDebugUtilsLabelEXT,
    vkCmdEndDebugUtilsLabelEXT :: VkCmdEndDebugUtilsLabelEXT,
    vkCmdInsertDebugUtilsLabelEXT :: VkCmdInsertDebugUtilsLabelEXT,
    vkCreateDebugUtilsMessengerEXT :: VkCreateDebugUtilsMessengerEXT,
    vkDestroyDebugUtilsMessengerEXT :: VkDestroyDebugUtilsMessengerEXT,
    vkQueueBeginDebugUtilsLabelEXT :: VkQueueBeginDebugUtilsLabelEXT,
    vkQueueEndDebugUtilsLabelEXT :: VkQueueEndDebugUtilsLabelEXT,
    vkQueueInsertDebugUtilsLabelEXT :: VkQueueInsertDebugUtilsLabelEXT,
    vkSetDebugUtilsObjectNameEXT :: VkSetDebugUtilsObjectNameEXT,
    vkSetDebugUtilsObjectTagEXT :: VkSetDebugUtilsObjectTagEXT,
    vkSubmitDebugUtilsMessageEXT :: VkSubmitDebugUtilsMessageEXT
  }

instance InstanceExtension VK_EXT_debug_utils where
  getInstanceExtension vkInstance = VK_EXT_debug_utils <$>
    getInstanceFun vkInstance vkFunCmdBeginDebugUtilsLabelEXT importVkCmdBeginDebugUtilsLabelEXT <*>
    getInstanceFun vkInstance vkFunCmdEndDebugUtilsLabelEXT importVkCmdEndDebugUtilsLabelEXT <*>
    getInstanceFun vkInstance vkFunCmdInsertDebugUtilsLabelEXT importVkCmdInsertDebugUtilsLabelEXT <*>
    getInstanceFun vkInstance vkFunCreateDebugUtilsMessengerEXT importVkCreateDebugUtilsMessengerEXT <*>
    getInstanceFun vkInstance vkFunDestroyDebugUtilsMessengerEXT importVkDestroyDebugUtilsMessengerEXT <*>
    getInstanceFun vkInstance vkFunQueueBeginDebugUtilsLabelEXT importVkQueueBeginDebugUtilsLabelEXT <*>
    getInstanceFun vkInstance vkFunQueueEndDebugUtilsLabelEXT importVkQueueEndDebugUtilsLabelEXT <*>
    getInstanceFun vkInstance vkFunQueueInsertDebugUtilsLabelEXT importVkQueueInsertDebugUtilsLabelEXT <*>
    getInstanceFun vkInstance vkFunSetDebugUtilsObjectNameEXT importVkSetDebugUtilsObjectNameEXT <*>
    getInstanceFun vkInstance vkFunSetDebugUtilsObjectTagEXT importVkSetDebugUtilsObjectTagEXT <*>
    getInstanceFun vkInstance vkFunSubmitDebugUtilsMessageEXT importVkSubmitDebugUtilsMessageEXT

foreign import capi "dynamic" importVkCmdBeginDebugUtilsLabelEXT :: DynamicImport VkCmdBeginDebugUtilsLabelEXT
foreign import capi "dynamic" importVkCmdEndDebugUtilsLabelEXT :: DynamicImport VkCmdEndDebugUtilsLabelEXT
foreign import capi "dynamic" importVkCmdInsertDebugUtilsLabelEXT :: DynamicImport VkCmdInsertDebugUtilsLabelEXT
foreign import capi "dynamic" importVkCreateDebugUtilsMessengerEXT :: DynamicImport VkCreateDebugUtilsMessengerEXT
foreign import capi "dynamic" importVkDestroyDebugUtilsMessengerEXT :: DynamicImport VkDestroyDebugUtilsMessengerEXT
foreign import capi "dynamic" importVkQueueBeginDebugUtilsLabelEXT :: DynamicImport VkQueueBeginDebugUtilsLabelEXT
foreign import capi "dynamic" importVkQueueEndDebugUtilsLabelEXT :: DynamicImport VkQueueEndDebugUtilsLabelEXT
foreign import capi "dynamic" importVkQueueInsertDebugUtilsLabelEXT :: DynamicImport VkQueueInsertDebugUtilsLabelEXT
foreign import capi "dynamic" importVkSetDebugUtilsObjectNameEXT :: DynamicImport VkSetDebugUtilsObjectNameEXT
foreign import capi "dynamic" importVkSetDebugUtilsObjectTagEXT :: DynamicImport VkSetDebugUtilsObjectTagEXT
foreign import capi "dynamic" importVkSubmitDebugUtilsMessageEXT :: DynamicImport VkSubmitDebugUtilsMessageEXT

foreign import ccall "wrapper" wrapPFN_vkDebugUtilsMessengerCallbackEXT :: DynamicWrapper PFN_vkDebugUtilsMessengerCallbackEXT

data DebugUtilsLabel =
  DebugUtilsLabel {
    withLabelNamePtr :: Codensity IO CString,
    color :: (Float, Float, Float, Float)
  }

instance MarshalAs VkDebugUtilsLabelEXT DebugUtilsLabel where
  marshalTo ptr fields = lowerCodensity do
    labelNamePtr <- fields.withLabelNamePtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
      pokePtrOffset @"pNext" nullPtr
      pokePtrOffset @"pLabelName" (castPtr labelNamePtr)
      pokePtrArrayOffset @"color" (let (r,g,b,a) = fields.color in [r,g,b,a])

data DebugUtilsMessengerCallbackData =
  DebugUtilsMessengerCallbackData {
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

instance MarshalAs VkDebugUtilsMessengerCallbackDataEXT DebugUtilsMessengerCallbackData where
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

data DebugUtilsObjectTagInfo =
  DebugUtilsObjectTagInfo {
    objectType :: VkObjectType,
    objectHandle :: Word64,
    tagName :: Word64,
    tagSize :: CSize,
    withTagPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugUtilsObjectTagInfoEXT DebugUtilsObjectTagInfo where
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

data DebugUtilsMessengerCreateInfo =
  DebugUtilsMessengerCreateInfo {
    withNextPtr :: Codensity IO (Ptr ()),
    messageSeverity :: VkDebugUtilsMessageSeverityFlagBitsEXT,
    messageType :: VkDebugUtilsMessageTypeFlagBitsEXT,
    userCallbackFunPtr :: FunPtr PFN_vkDebugUtilsMessengerCallbackEXT,
    withUserDataPtr :: Codensity IO (Ptr ())
  }

instance MarshalAs VkDebugUtilsMessengerCreateInfoEXT DebugUtilsMessengerCreateInfo where
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

data DebugUtilsObjectNameInfo =
  DebugUtilsObjectNameInfo {
    withNextPtr :: Codensity IO (Ptr ()),
    objectType :: VkObjectType,
    objectHandle :: Word64,
    withObjectNamePtr :: Codensity IO CString
  }

instance MarshalAs VkDebugUtilsObjectNameInfoEXT DebugUtilsObjectNameInfo where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    objectNamePtr <- fields.withObjectNamePtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"objectType" fields.objectType
      pokePtrOffset @"objectHandle" fields.objectHandle
      pokePtrOffset @"pObjectName" (castPtr objectNamePtr)

cmdBeginDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkCommandBuffer ->
  Codensity IO (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
cmdBeginDebugUtilsLabel ext commandBuffer withLabelInfoPtr = lowerCodensity do
  labelInfoPtr <- withLabelInfoPtr
  liftIO $ vkCmdBeginDebugUtilsLabelEXT ext commandBuffer labelInfoPtr

cmdEndDebugUtilsLabel :: VK_EXT_debug_utils -> VkCmdEndDebugUtilsLabelEXT
cmdEndDebugUtilsLabel = vkCmdEndDebugUtilsLabelEXT

cmdInsertDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkCommandBuffer ->
  Codensity IO (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
cmdInsertDebugUtilsLabel ext commandBuffer withLabelInfoPtr = lowerCodensity do
  labelInfoPtr <- withLabelInfoPtr
  liftIO $ vkCmdInsertDebugUtilsLabelEXT ext commandBuffer labelInfoPtr

createDebugUtilsMessenger ::
  VK_EXT_debug_utils ->
  VkInstance ->
  Codensity IO (Ptr VkDebugUtilsMessengerCreateInfoEXT) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO VkDebugUtilsMessengerEXT
createDebugUtilsMessenger ext vkInstance withCreateInfoPtr withAllocatorPtr = lowerCodensity do
  createInfoPtr <- withCreateInfoPtr
  allocatorPtr <- withAllocatorPtr
  ptr <- Codensity alloca
  liftIO do
    vkCreateDebugUtilsMessengerEXT ext vkInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateDebugUtilsMessengerEXT
    peek ptr

destroyDebugUtilsMessenger ::
  VK_EXT_debug_utils ->
  VkInstance ->
  VkDebugUtilsMessengerEXT ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO ()
destroyDebugUtilsMessenger ext vkInstance messenger withAllocatorPtr = lowerCodensity do
  allocatorPtr <- withAllocatorPtr
  liftIO $ vkDestroyDebugUtilsMessengerEXT ext vkInstance messenger allocatorPtr

debugUtilsMessengerResource ::
  VK_EXT_debug_utils ->
  VkInstance ->
  Codensity IO (Ptr VkDebugUtilsMessengerCreateInfoEXT) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Resource VkDebugUtilsMessengerEXT
debugUtilsMessengerResource ext vkInstance withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createDebugUtilsMessenger ext vkInstance withCreateInfoPtr withCreateAllocatorPtr)
  (\messenger -> destroyDebugUtilsMessenger ext vkInstance messenger withDestroyAllocatorPtr)

queueBeginDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkQueue ->
  Codensity IO (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
queueBeginDebugUtilsLabel ext queue withLabelInfoPtr = lowerCodensity do
  labelInfoPtr <- withLabelInfoPtr
  liftIO $ vkQueueBeginDebugUtilsLabelEXT ext queue labelInfoPtr

queueEndDebugUtilsLabel :: VK_EXT_debug_utils -> VkQueueEndDebugUtilsLabelEXT
queueEndDebugUtilsLabel = vkQueueEndDebugUtilsLabelEXT

queueInsertDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkQueue ->
  Codensity IO (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
queueInsertDebugUtilsLabel ext queue withLabelInfoPtr = lowerCodensity do
  labelInfoPtr <- withLabelInfoPtr
  liftIO $ vkQueueInsertDebugUtilsLabelEXT ext queue labelInfoPtr

setDebugUtilsObjectName ::
  VK_EXT_debug_utils ->
  VkDevice ->
  Codensity IO (Ptr VkDebugUtilsObjectNameInfoEXT) ->
  IO ()
setDebugUtilsObjectName ext device withNameInfoPtr = lowerCodensity do
  nameInfoPtr <- withNameInfoPtr
  liftIO $ vkSetDebugUtilsObjectNameEXT ext device nameInfoPtr >>=
    throwIfVkResultNotSuccess vkFunSetDebugUtilsObjectNameEXT

setDebugUtilsObjectTag ::
  VK_EXT_debug_utils ->
  VkDevice ->
  Codensity IO (Ptr VkDebugUtilsObjectTagInfoEXT) ->
  IO ()
setDebugUtilsObjectTag ext device withTagInfoPtr = lowerCodensity do
  nameInfoPtr <- withTagInfoPtr
  liftIO $ vkSetDebugUtilsObjectTagEXT ext device nameInfoPtr >>=
    throwIfVkResultNotSuccess vkFunSetDebugUtilsObjectTagEXT

submitDebugUtilsMessage ::
  VK_EXT_debug_utils ->
  VkInstance ->
  VkDebugUtilsMessageSeverityFlagBitsEXT ->
  VkDebugUtilsMessageTypeFlagsEXT ->
  Codensity IO (Ptr VkDebugUtilsMessengerCallbackDataEXT) ->
  IO ()
submitDebugUtilsMessage ext vkInstance messageSeverity messageTypes withCallbackDataPtr = lowerCodensity do
  callbackDataPtr <- withCallbackDataPtr
  liftIO $ vkSubmitDebugUtilsMessageEXT ext vkInstance messageSeverity messageTypes callbackDataPtr
