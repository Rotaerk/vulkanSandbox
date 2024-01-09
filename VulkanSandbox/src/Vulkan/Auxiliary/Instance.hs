{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Instance (
  VkApplicationInfoFields(..),
  VkInstanceCreateInfoFields(..),
  createVkInstance,
  destroyVkInstance,
  vkInstanceResource,
  getVkInstanceFun,
  VkInstanceExtension(..)
) where

import Control.Monad.Codensity
import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import MarshalAs
import ScopedResource
import Vulkan.Core_1_0
import Vulkan.Auxiliary.Exception

data VkApplicationInfoFields =
  VkApplicationInfoFields {
    withAppNamePtr :: Codensity IO CString,
    appVersion :: Word32,
    withEngineNamePtr :: Codensity IO CString,
    engineVersion :: Word32,
    apiVersion :: Word32
  }

instance MarshalAs VkApplicationInfo VkApplicationInfoFields where
  marshalTo ptr fields = lowerCodensity do
    appNamePtr <- fields.withAppNamePtr
    engineNamePtr <- fields.withEngineNamePtr
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      pokePtrOffset @"pNext" nullPtr
      pokePtrOffset @"pApplicationName" (castPtr appNamePtr)
      pokePtrOffset @"applicationVersion" fields.appVersion
      pokePtrOffset @"pEngineName" (castPtr engineNamePtr)
      pokePtrOffset @"engineVersion" fields.engineVersion
      pokePtrOffset @"apiVersion" fields.apiVersion

data VkInstanceCreateInfoFields =
  VkInstanceCreateInfoFields {
    withNextPtr :: Codensity IO (Ptr ()),
    flags :: VkInstanceCreateFlags,
    withAppInfoPtr :: Codensity IO (Ptr VkApplicationInfo),
    withEnabledLayerNamesPtrLen :: Codensity IO (Ptr CString, Word32),
    withEnabledExtensionNamesPtrLen :: Codensity IO (Ptr CString, Word32)
  }

instance MarshalAs VkInstanceCreateInfo VkInstanceCreateInfoFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- fields.withNextPtr
    appInfoPtr <- fields.withAppInfoPtr
    (enabledLayerNamesPtr, enabledLayerCount) <- fields.withEnabledLayerNamesPtrLen
    (enabledExtensionNamesPtr, enabledExtensionCount) <- fields.withEnabledExtensionNamesPtrLen
    liftIO $ runForPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" fields.flags
      pokePtrOffset @"pApplicationInfo" appInfoPtr
      pokePtrOffset @"enabledLayerCount" enabledLayerCount
      pokePtrOffset @"ppEnabledLayerNames" (castPtr enabledLayerNamesPtr)
      pokePtrOffset @"enabledExtensionCount" enabledExtensionCount
      pokePtrOffset @"ppEnabledExtensionNames" (castPtr enabledExtensionNamesPtr)

createVkInstance ::
  Codensity IO (Ptr VkInstanceCreateInfo) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO VkInstance
createVkInstance withCreateInfoPtr withAllocatorPtr = lowerCodensity do
  createInfoPtr <- withCreateInfoPtr
  allocatorPtr <- withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateInstance
    peek ptr

destroyVkInstance :: VkInstance -> Codensity IO (Ptr VkAllocationCallbacks) -> IO ()
destroyVkInstance vkInstance withAllocatorPtr = lowerCodensity do
  allocatorPtr <- withAllocatorPtr
  liftIO $ vkDestroyInstance vkInstance allocatorPtr

vkInstanceResource ::
  Codensity IO (Ptr VkInstanceCreateInfo) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Resource VkInstance
vkInstanceResource withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createVkInstance withCreateInfoPtr withCreateAllocatorPtr)
  (\vkInstance -> destroyVkInstance vkInstance withDestroyAllocatorPtr)

getVkInstanceFun :: VkInstance -> VkFun (VkInstance -> f) -> DynamicImport (VkInstance -> f) -> IO f
getVkInstanceFun vkInstance vkFun@(VkFun funNameCStr) dynImport = do
  funPtr <- vkGetInstanceFunPtr vkInstance vkFun
  when (funPtr == nullFunPtr) $ do
    funName <- peekCString funNameCStr
    throwVk ("Failed to obtain a pointer to instance function " ++ funName ++ ".")
  return $ dynImport funPtr vkInstance

class VkInstanceExtension a where
  getVkInstanceExtension :: VkInstance -> IO a
