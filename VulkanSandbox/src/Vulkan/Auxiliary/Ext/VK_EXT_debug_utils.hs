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

import Local.Control.Monad.Codensity
import Local.Foreign.Marshal.Alloc
import Local.Foreign.Marshal.Array
import Local.Foreign.Ptr

import Control.Monad
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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
      poke (offset @"pNext" ptr) nullPtr
      poke (offset @"pLabelName" ptr) $ castPtr labelNamePtr
      pokeArray (offset @"color" ptr) $ let (r,g,b,a) = fields.color in [r,g,b,a]

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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
      poke (offset @"pNext" ptr) nextPtr
      poke (offset @"flags" ptr) 0
      poke (offset @"pMessageIdName" ptr) $ castPtr messageIdNamePtr
      poke (offset @"messageIdNumber" ptr) fields.messageIdNumber
      poke (offset @"pMessage" ptr) $ castPtr messagePtr
      poke (offset @"queueLabelCount" ptr) fields.queueLabelCount
      poke (offset @"pQueueLabels" ptr) queueLabelsPtr
      poke (offset @"cmdBufLabelCount" ptr) fields.cmdBufLabelCount
      poke (offset @"pCmdBufLabels" ptr) cmdBufLabelsPtr
      poke (offset @"objectCount" ptr) fields.objectCount
      poke (offset @"pObjects" ptr) objectsPtr

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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
      poke (offset @"pNext" ptr) nullPtr
      poke (offset @"objectType" ptr) fields.objectType
      poke (offset @"objectHandle" ptr) fields.objectHandle
      poke (offset @"tagName" ptr) fields.tagName
      poke (offset @"tagSize" ptr) $ let (CSize w) = fields.tagSize in w
      poke (offset @"pTag" ptr) tagPtr

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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
      poke (offset @"pNext" ptr) nextPtr
      poke (offset @"flags" ptr) 0
      poke (offset @"messageSeverity" ptr) fields.messageSeverity
      poke (offset @"messageType" ptr) fields.messageType
      poke (offset @"pfnUserCallback" ptr) fields.userCallbackFunPtr
      poke (offset @"pUserData" ptr) userDataPtr

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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
      poke (offset @"pNext" ptr) nextPtr
      poke (offset @"objectType" ptr) fields.objectType
      poke (offset @"objectHandle" ptr) fields.objectHandle
      poke (offset @"pObjectName" ptr) $ castPtr objectNamePtr

cmdBeginDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkCommandBuffer ->
  SomeIOWith (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
cmdBeginDebugUtilsLabel ext commandBuffer withLabelInfoPtr =
  withLabelInfoPtr $ vkCmdBeginDebugUtilsLabelEXT ext commandBuffer

cmdEndDebugUtilsLabel :: VK_EXT_debug_utils -> VkCmdEndDebugUtilsLabelEXT
cmdEndDebugUtilsLabel = vkCmdEndDebugUtilsLabelEXT

cmdInsertDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkCommandBuffer ->
  SomeIOWith (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
cmdInsertDebugUtilsLabel ext commandBuffer withLabelInfoPtr =
  withLabelInfoPtr $ vkCmdInsertDebugUtilsLabelEXT ext commandBuffer

createDebugUtilsMessenger ::
  VK_EXT_debug_utils ->
  VkInstance ->
  SomeIOWith (Ptr VkDebugUtilsMessengerCreateInfoEXT) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  IO VkDebugUtilsMessengerEXT
createDebugUtilsMessenger ext vkInstance withCreateInfoPtr withAllocatorPtr =
  withCreateInfoPtr \createInfoPtr ->
  withAllocatorPtr \allocatorPtr ->
  allocaPeek \ptr -> do
    vkCreateDebugUtilsMessengerEXT ext vkInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateDebugUtilsMessengerEXT

destroyDebugUtilsMessenger ::
  VK_EXT_debug_utils ->
  VkInstance ->
  VkDebugUtilsMessengerEXT ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  IO ()
destroyDebugUtilsMessenger ext vkInstance messenger withAllocatorPtr =
  withAllocatorPtr $ vkDestroyDebugUtilsMessengerEXT ext vkInstance messenger

debugUtilsMessengerResource ::
  VK_EXT_debug_utils ->
  VkInstance ->
  SomeIOWith (Ptr VkDebugUtilsMessengerCreateInfoEXT) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  Resource VkDebugUtilsMessengerEXT
debugUtilsMessengerResource ext vkInstance withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createDebugUtilsMessenger ext vkInstance withCreateInfoPtr withCreateAllocatorPtr)
  (\messenger -> destroyDebugUtilsMessenger ext vkInstance messenger withDestroyAllocatorPtr)

queueBeginDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkQueue ->
  SomeIOWith (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
queueBeginDebugUtilsLabel ext queue withLabelInfoPtr =
  withLabelInfoPtr $ vkQueueBeginDebugUtilsLabelEXT ext queue

queueEndDebugUtilsLabel :: VK_EXT_debug_utils -> VkQueueEndDebugUtilsLabelEXT
queueEndDebugUtilsLabel = vkQueueEndDebugUtilsLabelEXT

queueInsertDebugUtilsLabel ::
  VK_EXT_debug_utils ->
  VkQueue ->
  SomeIOWith (Ptr VkDebugUtilsLabelEXT) ->
  IO ()
queueInsertDebugUtilsLabel ext queue withLabelInfoPtr =
  withLabelInfoPtr $ vkQueueInsertDebugUtilsLabelEXT ext queue

setDebugUtilsObjectName ::
  VK_EXT_debug_utils ->
  VkDevice ->
  SomeIOWith (Ptr VkDebugUtilsObjectNameInfoEXT) ->
  IO ()
setDebugUtilsObjectName ext device withNameInfoPtr =
  withNameInfoPtr (vkSetDebugUtilsObjectNameEXT ext device) >>=
    throwIfVkResultNotSuccess vkFunSetDebugUtilsObjectNameEXT

setDebugUtilsObjectTag ::
  VK_EXT_debug_utils ->
  VkDevice ->
  SomeIOWith (Ptr VkDebugUtilsObjectTagInfoEXT) ->
  IO ()
setDebugUtilsObjectTag ext device withTagInfoPtr =
  withTagInfoPtr (vkSetDebugUtilsObjectTagEXT ext device) >>=
    throwIfVkResultNotSuccess vkFunSetDebugUtilsObjectTagEXT

submitDebugUtilsMessage ::
  VK_EXT_debug_utils ->
  VkInstance ->
  VkDebugUtilsMessageSeverityFlagBitsEXT ->
  VkDebugUtilsMessageTypeFlagsEXT ->
  SomeIOWith (Ptr VkDebugUtilsMessengerCallbackDataEXT) ->
  IO ()
submitDebugUtilsMessage ext vkInstance messageSeverity messageTypes withCallbackDataPtr =
  withCallbackDataPtr $ vkSubmitDebugUtilsMessageEXT ext vkInstance messageSeverity messageTypes
