{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}

module Vulkan.Auxiliary.Core.Instance (
  VkApplicationInfoFields(..),
  withVkApplicationInfoPtr,
  VkInstanceCreateInfoFields(..),
  withVkInstanceCreateInfoPtr,
  createVkInstance,
  destroyVkInstance,
  vkInstanceResource,
  ImplicitVkInstance,
  theVkInstance,
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

data VkApplicationInfoFields =
  VkApplicationInfoFields {
    withAppNamePtr :: SomeIOCPS CString,
    appVersion :: Word32,
    withEngineNamePtr :: SomeIOCPS CString,
    engineVersion :: Word32,
    apiVersion :: Word32
  }

withVkApplicationInfoPtr :: VkApplicationInfoFields -> SomeIOCPS (Ptr VkApplicationInfo)
withVkApplicationInfoPtr info = runContT do
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

data VkInstanceCreateInfoFields =
  VkInstanceCreateInfoFields {
    withNextPtr :: SomeIOCPS (Ptr ()),
    flags :: VkInstanceCreateFlags,
    withAppInfoPtr :: SomeIOCPS (Ptr VkApplicationInfo),
    withEnabledLayerNamesPtrLen :: SomeIOCPS (Ptr CString, Word32),
    withEnabledExtensionNamesPtrLen :: SomeIOCPS (Ptr CString, Word32)
  }

withVkInstanceCreateInfoPtr :: VkInstanceCreateInfoFields -> SomeIOCPS (Ptr VkInstanceCreateInfo)
withVkInstanceCreateInfoPtr info = runContT $ do
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

createVkInstance ::
  SomeIOCPS (Ptr VkInstanceCreateInfo) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  IO VkInstance
createVkInstance withCreateInfoPtr withAllocatorPtr = evalContT $ do
  createInfoPtr <- ContT withCreateInfoPtr
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkCreateInstance createInfoPtr allocatorPtr ptr >>=
      throwIfVkResultNotSuccess vkFunCreateInstance
    peek ptr

destroyVkInstance :: VkInstance -> SomeIOCPS (Ptr VkAllocationCallbacks) -> IO ()
destroyVkInstance vkInstance withAllocatorPtr = evalContT $ do
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

type ImplicitVkInstance = (?vkInstance :: VkInstance)

theVkInstance :: ImplicitVkInstance => VkInstance
theVkInstance = ?vkInstance

getVkInstanceFun :: ImplicitVkInstance => VkFun f -> DynamicImport f -> IO f
getVkInstanceFun vkFun@(VkFun funNameCStr) dynImport = do
  funPtr <- vkGetInstanceFunPtr theVkInstance vkFun
  when (funPtr == nullFunPtr) $ do
    funName <- peekCString funNameCStr
    throwVk ("Failed to obtain a pointer to instance function " ++ funName ++ ".")
  return $ dynImport funPtr

class VkInstanceExtension a where
  getVkInstanceExtension :: ImplicitVkInstance => IO a
