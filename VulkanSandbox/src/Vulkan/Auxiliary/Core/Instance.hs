{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Core.Instance (
  VkApplicationInfoFields(..),
  VkInstanceCreateInfoFields(..),
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
import Vulkan.Auxiliary.StructFields

data VkApplicationInfoFields r =
  VkApplicationInfoFields {
    withAppNamePtr :: IOWith CString r,
    appVersion :: Word32,
    withEngineNamePtr :: IOWith CString r,
    engineVersion :: Word32,
    apiVersion :: Word32
  }

instance VkStructFields VkApplicationInfoFields VkApplicationInfo where
  withVkStructPtr fields = runContT do
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
    withNextPtr :: IOWith (Ptr ()) r,
    flags :: VkInstanceCreateFlags,
    withAppInfoPtr :: IOWith (Ptr VkApplicationInfo) r,
    withEnabledLayerNamesPtrLen :: IOWith (Ptr CString, Word32) r,
    withEnabledExtensionNamesPtrLen :: IOWith (Ptr CString, Word32) r
  }

instance VkStructFields VkInstanceCreateInfoFields VkInstanceCreateInfo where
  withVkStructPtr fields = runContT do
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
  SomeIOWith (Ptr VkInstanceCreateInfo) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  IO VkInstance
createVkInstance withCreateInfoPtr withAllocatorPtr = evalContT do
  createInfoPtr <- ContT withCreateInfoPtr
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateInstance
    peek ptr

destroyVkInstance :: VkInstance -> SomeIOWith (Ptr VkAllocationCallbacks) -> IO ()
destroyVkInstance vkInstance withAllocatorPtr = evalContT do
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ vkDestroyInstance vkInstance allocatorPtr

vkInstanceResource ::
  SomeIOWith (Ptr VkInstanceCreateInfo) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
  SomeIOWith (Ptr VkAllocationCallbacks) ->
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
