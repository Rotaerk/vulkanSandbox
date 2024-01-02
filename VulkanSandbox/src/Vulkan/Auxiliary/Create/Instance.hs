{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Create.Instance (
  VkaApplicationInfo(..),
  vkaWithApplicationInfoPtr,
  VkaInstanceCreateInfo(..),
  vkaWithInstanceCreateInfoPtr,
  vkaCreateInstance,
  vkaDestroyInstance,
  vkaCreateScopedInstance
) where

import Local.Control.Monad.Cont
import Local.Foreign.Marshal.Alloc
import Local.Foreign.Storable.Offset

import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.Ptr
import Scope
import Vulkan.Core_1_0
import Vulkan.Auxiliary.Exception

data VkaApplicationInfo =
  VkaApplicationInfo {
    withAppNamePtr :: forall a. IOWith CString a,
    appVersion :: Word32,
    withEngineNamePtr :: forall a. IOWith CString a,
    engineVersion :: Word32,
    apiVersion :: Word32
  }

vkaWithApplicationInfoPtr :: VkaApplicationInfo -> IOWith (Ptr VkApplicationInfo) a
vkaWithApplicationInfoPtr info = runContT do
  appInfoPtr <- ContT $ alloca @VkApplicationInfo
  appNamePtr <- ContT $ withAppNamePtr info
  engineNamePtr <- ContT $ withEngineNamePtr info
  liftIO $ withImplicitPtr appInfoPtr do
    pokePtrOffset @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
    pokePtrOffset @"pNext" nullPtr
    pokePtrOffset @"pApplicationName" (castPtr appNamePtr)
    pokePtrOffset @"applicationVersion" (appVersion info)
    pokePtrOffset @"pEngineName" (castPtr engineNamePtr)
    pokePtrOffset @"engineVersion" (engineVersion info)
    pokePtrOffset @"apiVersion" (apiVersion info)
  return appInfoPtr

data VkaInstanceCreateInfo =
  VkaInstanceCreateInfo {
    withNextPtr :: forall a. IOWith (Ptr ()) a,
    flags :: VkInstanceCreateFlags,
    withAppInfoPtr :: forall a. IOWith (Ptr VkApplicationInfo) a,
    withEnabledLayerNamesPtrLen :: forall a. IOWith (Ptr CString, Word32) a,
    withEnabledExtensionNamesPtrLen :: forall a. IOWith (Ptr CString, Word32) a
  }

vkaWithInstanceCreateInfoPtr :: VkaInstanceCreateInfo -> IOWith (Ptr VkInstanceCreateInfo) a
vkaWithInstanceCreateInfoPtr info = runContT $ do
  createInfoPtr <- ContT $ alloca @VkInstanceCreateInfo
  nextPtr <- ContT $ withNextPtr info
  appInfoPtr <- ContT $ withAppInfoPtr info
  (enabledLayerNamesPtr, enabledLayerCount) <- ContT $ withEnabledLayerNamesPtrLen info
  (enabledExtensionNamesPtr, enabledExtensionCount) <- ContT $ withEnabledExtensionNamesPtrLen info
  liftIO $ withImplicitPtr createInfoPtr do
    pokePtrOffset @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
    pokePtrOffset @"pNext" nextPtr
    pokePtrOffset @"flags" (flags info)
    pokePtrOffset @"pApplicationInfo" appInfoPtr
    pokePtrOffset @"enabledLayerCount" enabledLayerCount
    pokePtrOffset @"ppEnabledLayerNames" (castPtr enabledLayerNamesPtr)
    pokePtrOffset @"enabledExtensionCount" enabledExtensionCount
    pokePtrOffset @"ppEnabledExtensionNames" (castPtr enabledExtensionNamesPtr)
  return createInfoPtr

vkaCreateInstance ::
  (forall a. IOWith (Ptr VkInstanceCreateInfo) a) ->
  (forall a. IOWith (Ptr VkAllocationCallbacks) a) ->
  IO VkInstance
vkaCreateInstance withCreateInfoPtr withAllocatorPtr = evalContT $ do
  createInfoPtr <- ContT withCreateInfoPtr
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ allocaPeek \ptr ->
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
    vkaThrowIfResultNotSuccess "vkCreateInstance"

vkaDestroyInstance ::
  VkInstance ->
  (forall a. IOWith (Ptr VkAllocationCallbacks) a) ->
  IO ()
vkaDestroyInstance vulkanInstance withAllocatorPtr = evalContT $ do
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ vkDestroyInstance vulkanInstance allocatorPtr

vkaCreateScopedInstance ::
  ImplicitScope s =>
  (forall a. IOWith (Ptr VkInstanceCreateInfo) a) ->
  (forall a. IOWith (Ptr VkAllocationCallbacks) a) ->
  (forall a. IOWith (Ptr VkAllocationCallbacks) a) ->
  IO VkInstance
vkaCreateScopedInstance withCreateInfoPtr withCreateAllocatorPtr withDestroyAllocatorPtr =
  scoped
    (vkaCreateInstance withCreateInfoPtr withCreateAllocatorPtr)
    (\vulkanInstance -> vkaDestroyInstance vulkanInstance withDestroyAllocatorPtr)
