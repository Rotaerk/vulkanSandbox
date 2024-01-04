{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Core.Instance (
  VkApplicationInfoFields(..),
  withVkApplicationInfoPtr,
  VkInstanceCreateInfoFields(..),
  withVkInstanceCreateInfoPtr,
  createVkInstance,
  destroyVkInstance,
  vkInstanceResource,
  getVkInstanceFun,
  VkInstanceExtension(..)
) where

import Local.Control.Monad.Cont
import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad
import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import ScopedResource
import Vulkan.Core_1_0
import Vulkan.Auxiliary.Exception

data VkApplicationInfoFields r =
  VkApplicationInfoFields {
    withAppNamePtr :: IOCPS r CString,
    appVersion :: Word32,
    withEngineNamePtr :: IOCPS r CString,
    engineVersion :: Word32,
    apiVersion :: Word32
  }

withVkApplicationInfoPtr :: VkApplicationInfoFields r -> IOCPS r (Ptr VkApplicationInfo)
withVkApplicationInfoPtr fields = runContT do
  appInfoPtr <- ContT $ alloca @VkApplicationInfo
  appNamePtr <- ContT fields.withAppNamePtr
  engineNamePtr <- ContT fields.withEngineNamePtr
  liftIO $ withImplicitPtr appInfoPtr do
    pokePtrOffset @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
    pokePtrOffset @"pNext" nullPtr
    pokePtrOffset @"pApplicationName" (castPtr appNamePtr)
    pokePtrOffset @"applicationVersion" fields.appVersion
    pokePtrOffset @"pEngineName" (castPtr engineNamePtr)
    pokePtrOffset @"engineVersion" fields.engineVersion
    pokePtrOffset @"apiVersion" fields.apiVersion
  return appInfoPtr

data VkInstanceCreateInfoFields r =
  VkInstanceCreateInfoFields {
    withNextPtr :: IOCPS r (Ptr ()),
    flags :: VkInstanceCreateFlags,
    withAppInfoPtr :: IOCPS r (Ptr VkApplicationInfo),
    withEnabledLayerNamesPtrLen :: IOCPS r (Ptr CString, Word32),
    withEnabledExtensionNamesPtrLen :: IOCPS r (Ptr CString, Word32)
  }

withVkInstanceCreateInfoPtr :: VkInstanceCreateInfoFields r -> IOCPS r (Ptr VkInstanceCreateInfo)
withVkInstanceCreateInfoPtr fields = runContT do
  createInfoPtr <- ContT $ alloca @VkInstanceCreateInfo
  nextPtr <- ContT fields.withNextPtr
  appInfoPtr <- ContT fields.withAppInfoPtr
  (enabledLayerNamesPtr, enabledLayerCount) <- ContT fields.withEnabledLayerNamesPtrLen
  (enabledExtensionNamesPtr, enabledExtensionCount) <- ContT fields.withEnabledExtensionNamesPtrLen
  liftIO $ withImplicitPtr createInfoPtr do
    pokePtrOffset @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
    pokePtrOffset @"pNext" nextPtr
    pokePtrOffset @"flags" fields.flags
    pokePtrOffset @"pApplicationInfo" appInfoPtr
    pokePtrOffset @"enabledLayerCount" enabledLayerCount
    pokePtrOffset @"ppEnabledLayerNames" (castPtr enabledLayerNamesPtr)
    pokePtrOffset @"enabledExtensionCount" enabledExtensionCount
    pokePtrOffset @"ppEnabledExtensionNames" (castPtr enabledExtensionNamesPtr)
  return createInfoPtr

createVkInstance ::
  SomeIOCPS (Ptr VkInstanceCreateInfo) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  IO VkInstance
createVkInstance withCreateInfoPtr withAllocatorPtr = evalContT do
  createInfoPtr <- ContT withCreateInfoPtr
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateInstance
    peek ptr

destroyVkInstance :: VkInstance -> SomeIOCPS (Ptr VkAllocationCallbacks) -> IO ()
destroyVkInstance vkInstance withAllocatorPtr = evalContT do
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ vkDestroyInstance vkInstance allocatorPtr

vkInstanceResource ::
  SomeIOCPS (Ptr VkInstanceCreateInfo) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
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
