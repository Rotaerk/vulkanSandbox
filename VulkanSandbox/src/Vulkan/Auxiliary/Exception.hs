module Vulkan.Auxiliary.Exception (
  VkResultException(..),
  throwIfVkResultNotIn,
  throwIfVkResultNotSuccess,
  VkException(..),
  throwVk,
  throwVkM
) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Vulkan.Core_1_0

data VkResultException =
  VkResultException {
    vkResultException'functionName :: String,
    vkResultException'result :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkResultException where
  displayException (VkResultException functionName result) =
    "Vulkan function " ++ functionName ++ " failed with result: " ++ show result

throwIfVkResultNotIn :: [VkResult] -> String -> VkResult -> IO VkResult
throwIfVkResultNotIn successResults functionName result
  | result `elem` successResults = return result
  | otherwise = throwIO (VkResultException functionName result)

throwIfVkResultNotSuccess :: String -> VkResult -> IO ()
throwIfVkResultNotSuccess functionName result =
  void $ throwIfVkResultNotIn [VK_SUCCESS] functionName result

data VkException = VkException String deriving (Eq, Show, Read)

instance Exception VkException where
  displayException (VkException message) = "Vulkan exception: " ++ message

throwVk :: String -> a
throwVk = throw . VkException

throwVkM :: MonadThrow m => String -> m a
throwVkM = throwM . VkException
