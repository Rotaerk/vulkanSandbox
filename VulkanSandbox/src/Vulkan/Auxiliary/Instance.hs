{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vulkan.Auxiliary.Instance (
  VkApplicationInfoFields(..),
  VkInstanceCreateInfoFields(..),
  createVkInstance,
  destroyVkInstance,
  vkInstanceResource,
  getVkInstanceFun,
  VkInstanceExtension(..)
) where

import Local.Foreign.Ptr
import Local.Foreign.Storable.Offset

import Control.Monad
import Control.Monad.Codensity
import Control.Monad.IO.Class
import Data.Word
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import MarshalAs
import ScopedResource
import Vulkan.Auxiliary.Core
import Vulkan.Auxiliary.Exception

data VkApplicationInfoFields =
  VkApplicationInfoFields {
    aif'withAppNamePtr :: Codensity IO CString,
    aif'appVersion :: Word32,
    aif'withEngineNamePtr :: Codensity IO CString,
    aif'engineVersion :: Word32,
    aif'apiVersion :: Word32
  }

instance MarshalAs VkApplicationInfo VkApplicationInfoFields where
  marshalTo ptr fields = lowerCodensity do
    appNamePtr <- aif'withAppNamePtr fields
    engineNamePtr <- aif'withEngineNamePtr fields
    liftIO $ withImplicitPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
      pokePtrOffset @"pNext" nullPtr
      pokePtrOffset @"pApplicationName" (castPtr appNamePtr)
      pokePtrOffset @"applicationVersion" (aif'appVersion fields)
      pokePtrOffset @"pEngineName" (castPtr engineNamePtr)
      pokePtrOffset @"engineVersion" (aif'engineVersion fields)
      pokePtrOffset @"apiVersion" (aif'apiVersion fields)

data VkInstanceCreateInfoFields =
  VkInstanceCreateInfoFields {
    icif'withNextPtr :: Codensity IO (Ptr ()),
    icif'flags :: VkInstanceCreateFlags,
    icif'withAppInfoPtr :: Codensity IO (Ptr VkApplicationInfo),
    icif'withEnabledLayerNamesPtrLen :: Codensity IO (Ptr CString, Word32),
    icif'withEnabledExtensionNamesPtrLen :: Codensity IO (Ptr CString, Word32)
  }

instance MarshalAs VkInstanceCreateInfo VkInstanceCreateInfoFields where
  marshalTo ptr fields = lowerCodensity do
    nextPtr <- icif'withNextPtr fields
    appInfoPtr <- icif'withAppInfoPtr fields
    (enabledLayerNamesPtr, enabledLayerCount) <- icif'withEnabledLayerNamesPtrLen fields
    (enabledExtensionNamesPtr, enabledExtensionCount) <- icif'withEnabledExtensionNamesPtrLen fields
    liftIO $ withImplicitPtr ptr do
      pokePtrOffset @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
      pokePtrOffset @"pNext" nextPtr
      pokePtrOffset @"flags" (icif'flags fields)
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
  (\vkInstance -> destroyVkInstance vkInstance withDestroyAllocatorPtr >> putStrLn "VkInstance destroyed.")

getVkInstanceFun :: VkInstance -> VkFun (VkInstance -> f) -> DynamicImport (VkInstance -> f) -> IO f
getVkInstanceFun vkInstance vkFun@(VkFun funNameCStr) dynImport = do
  funPtr <- vkGetInstanceFunPtr vkInstance vkFun
  when (funPtr == nullFunPtr) $ do
    funName <- peekCString funNameCStr
    throwVk ("Failed to obtain a pointer to instance function " ++ funName ++ ".")
  return $ dynImport funPtr vkInstance

class VkInstanceExtension a where
  getVkInstanceExtension :: VkInstance -> IO a
