{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CApiFFI #-}

module Vulkan.Auxiliary.Ext.VK_EXT_debug_report (
  module Vulkan.Ext.VK_EXT_debug_report
) where

import Local.Control.Monad.Cont
import Local.Foreign.Ptr

import Control.Monad.IO.Class
import Foreign.Marshal.Alloc
import Foreign.Storable
import Vulkan.Auxiliary
import Vulkan.Ext.VK_EXT_debug_report

-- Need a function that obtains this extension and stores its functions somewhere.
-- Then I will make that collection of functions available via an implicit param.
-- I will eventually want to generalize that into a type class with an instance per extension
-- and whose method loads that extension's functions.
{-
vkaCreateDebugReportCallbackEXT ::
  VkaImplicitInstance =>
-- VkaImplicitExtDebugReport =>
  SomeIOCPS (Ptr VkDebugReportCallbackCreateInfoEXT) ->
  SomeIOCPS (Ptr VkAllocationCallbacks) ->
  IO ()
vkaCreateDebugReportCallbackEXT withCreateInfoPtr withAllocatorPtr = evalContT $ do
  createInfoPtr <- ContT withCreateInfoPtr
  allocatorPtr <- ContT withAllocatorPtr
  liftIO $ alloca \ptr -> do
    vkFunCreateDebugReportCallbackEXT createInfoPtr allocatorPtr ptr >>=
      vkaThrowIfResultNotSuccess "vkFunCreateDebugReportCallbackEXT"
    peek ptr
-}

foreign import capi "dynamic" dynVkCreateDebugReportCallbackEXT :: DynamicImport VkCreateDebugReportCallbackEXT
foreign import capi "dynamic" dynVkDebugReportMessageEXT :: DynamicImport VkDebugReportMessageEXT
foreign import capi "dynamic" dynVkDestroyDebugReportCallbackEXT :: DynamicImport VkDestroyDebugReportCallbackEXT

-- The structure of this create function is much like the instance one.  This can probably be generalized to something like:
-- vkaCreate ::
--   SomeIOCPS (Ptr createInfo) ->
--   SomeIOCPS (Ptr VkAllocationCallbacks) ->
--   IO (VkResult, vk)

-- and for ones that only have VK_SUCCESS as a success result:
-- vkaCreateSimple ::
--   SomeIOCPS (Ptr createInfo) ->
--   SomeIOCPS (Ptr VkAllocationCallbacks) ->
--   IO vkg
