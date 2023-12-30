module Vulkan.Auxiliary.Exception (
  VkaResultException(..),
  vkaThrowIfResultNotIn,
  vkaThrowIfResultNotSuccess,
  VkaException(..),
  vkaThrow,
  vkaThrowM
) where

import Control.Exception
import Control.Monad
import Control.Monad.Catch
import Vulkan.Core_1_0

data VkaResultException =
  VkaResultException {
    vkaResultException'functionName :: String,
    vkaResultException'result :: VkResult
  } deriving (Eq, Show, Read)

instance Exception VkaResultException where
  displayException (VkaResultException functionName result) =
    "Vulkan function " ++ functionName ++ " failed with result: " ++ show result

vkaThrowIfResultNotIn :: [VkResult] -> String -> VkResult -> IO VkResult
vkaThrowIfResultNotIn successResults functionName result
  | result `elem` successResults = return result
  | otherwise = throwIO (VkaResultException functionName result)

vkaThrowIfResultNotSuccess :: String -> VkResult -> IO ()
vkaThrowIfResultNotSuccess functionName result =
  void $ vkaThrowIfResultNotIn [VK_SUCCESS] functionName result

data VkaException = VkaException String deriving (Eq, Show, Read)

instance Exception VkaException where
  displayException (VkaException message) = "Vulkan exception: " ++ message

vkaThrow :: String -> a
vkaThrow = throw . VkaException

vkaThrowM :: MonadThrow m => String -> m a
vkaThrowM = throwM . VkaException
