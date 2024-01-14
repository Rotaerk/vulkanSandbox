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

import Local.Control.Monad.Codensity
import Local.Foreign.Marshal.Alloc
import Local.Foreign.Ptr

import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.Storable
import Foreign.Storable.Offset
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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_APPLICATION_INFO
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_APPLICATION_INFO
      poke (offset @"pNext" ptr) nullPtr
      poke (offset @"pApplicationName" ptr) $ castPtr appNamePtr
      poke (offset @"applicationVersion" ptr) fields.appVersion
      poke (offset @"pEngineName" ptr) $ castPtr engineNamePtr
      poke (offset @"engineVersion" ptr) fields.engineVersion
      poke (offset @"apiVersion" ptr) fields.apiVersion

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
    liftIO do
      poke (offset @"sType" ptr) VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      poke (offset @"pNext" ptr) nextPtr
      poke (offset @"flags" ptr) fields.flags
      poke (offset @"pApplicationInfo" ptr) appInfoPtr
      poke (offset @"enabledLayerCount" ptr) enabledLayerCount
      poke (offset @"ppEnabledLayerNames" ptr) $ castPtr enabledLayerNamesPtr
      poke (offset @"enabledExtensionCount" ptr) enabledExtensionCount
      poke (offset @"ppEnabledExtensionNames" ptr) $ castPtr enabledExtensionNamesPtr

createInstance ::
  SomeIOWith (Ptr VkInstanceCreateInfo) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  IO VkInstance
createInstance withCreateInfoPtr withAllocatorPtr =
  withCreateInfoPtr \createInfoPtr ->
  withAllocatorPtr \allocatorPtr ->
  allocaPeek \ptr -> do
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateInstance

destroyInstance :: VkInstance -> SomeIOWith (Ptr VkAllocationCallbacks) -> IO ()
destroyInstance vkInstance withAllocatorPtr = withAllocatorPtr $ vkDestroyInstance vkInstance

instanceResource ::
  SomeIOWith (Ptr VkInstanceCreateInfo) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
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
