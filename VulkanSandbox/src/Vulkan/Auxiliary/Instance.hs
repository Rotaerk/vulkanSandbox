{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Instance (
  ApplicationInfo(..),
  InstanceCreateInfo(..),
  createInstance,
  destroyInstance,
  instanceResource,
  getInstanceFun,
  InstanceExtension(..)
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

data ApplicationInfo =
  ApplicationInfo {
    withAppNamePtr :: Codensity IO CString,
    appVersion :: Word32,
    withEngineNamePtr :: Codensity IO CString,
    engineVersion :: Word32,
    apiVersion :: Word32
  }

instance MarshalAs VkApplicationInfo ApplicationInfo where
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

data InstanceCreateInfo =
  InstanceCreateInfo {
    withNextPtr :: Codensity IO (Ptr ()),
    flags :: VkInstanceCreateFlags,
    withAppInfoPtr :: Codensity IO (Ptr VkApplicationInfo),
    withEnabledLayerNamesPtrLen :: Codensity IO (Ptr CString, Word32),
    withEnabledExtensionNamesPtrLen :: Codensity IO (Ptr CString, Word32)
  }

instance MarshalAs VkInstanceCreateInfo InstanceCreateInfo where
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

createInstance ::
  Codensity IO (Ptr VkInstanceCreateInfo) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  IO VkInstance
createInstance withCreateInfoPtr withAllocatorPtr = lowerCodensity do
  createInfoPtr <- withCreateInfoPtr
  allocatorPtr <- withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateInstance
    peek ptr

destroyInstance :: VkInstance -> Codensity IO (Ptr VkAllocationCallbacks) -> IO ()
destroyInstance vkInstance withAllocatorPtr = lowerCodensity do
  allocatorPtr <- withAllocatorPtr
  liftIO $ vkDestroyInstance vkInstance allocatorPtr

instanceResource ::
  Codensity IO (Ptr VkInstanceCreateInfo) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Codensity IO (Ptr VkAllocationCallbacks) ->
  Resource VkInstance
instanceResource withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr = Resource
  (createInstance withCreateInfoPtr withCreateAllocatorPtr)
  (\vkInstance -> destroyInstance vkInstance withDestroyAllocatorPtr)

getInstanceFun :: VkInstance -> VkFun f -> DynamicImport f -> IO f
getInstanceFun vkInstance vkFun@(VkFun funNameCStr) dynImport = do
  funPtr <- vkGetInstanceFunPtr vkInstance vkFun
  when (funPtr == nullFunPtr) $ do
    funName <- peekCString funNameCStr
    throwVk ("Failed to obtain a pointer to instance function " ++ funName ++ ".")
  return $ dynImport funPtr

class InstanceExtension a where
  getInstanceExtension :: VkInstance -> IO a
