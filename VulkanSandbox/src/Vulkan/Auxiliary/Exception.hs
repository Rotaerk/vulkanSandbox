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
import Foreign.C.String
import Vulkan.Core_1_0

data VkResultException =
  VkResultException {
    vkResultException'functionName :: String,
    vkResultException'result :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkResultException where
  displayException (VkResultException functionName result) =
    "Vulkan function " ++ functionName ++ " failed with result: " ++ show result

throwIfVkResultNotIn :: [VkResult] -> VkFun f -> VkResult -> IO VkResult
throwIfVkResultNotIn successResults vkFun result
  | result `elem` successResults = return result
  | otherwise = do
      let (VkFun funNameCStr) = vkFun
      functionName <- peekCString funNameCStr
      throwIO (VkResultException functionName result)

throwIfVkResultNotSuccess :: VkFun f -> VkResult -> IO ()
throwIfVkResultNotSuccess vkFun result =
  void $ throwIfVkResultNotIn [VK_SUCCESS] vkFun result

data VkException = VkException String deriving (Eq, Show, Read)

instance Exception VkException where
  displayException (VkException message) = "Vulkan exception: " ++ message

throwVk :: String -> a
throwVk = throw . VkException

throwVkM :: MonadThrow m => String -> m a
throwVkM = throwM . VkException
