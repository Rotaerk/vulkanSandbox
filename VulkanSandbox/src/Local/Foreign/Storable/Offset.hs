{-# LANGUAGE AllowAmbiguousTypes #-}

module Local.Foreign.Storable.Offset (
  module Foreign.Storable.Offset,
  runForPtr,
  pokePtrOffset,
  pokePtrArrayOffset
) where

import Local.Control.Monad

import Control.Monad.IO.Class
import Control.Monad.Reader
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Offset
import GHC.Records

runForPtr :: Ptr r -> ReaderT (Ptr r) IO a -> IO a
runForPtr ptr reader = runReaderT reader ptr

pokePtrOffset ::
  forall x r a.
  (HasField x r a, Offset x r, Storable a) =>
  a -> ReaderT (Ptr r) IO ()
pokePtrOffset value = do
  ptr <- ask
  liftIO $ poke (offset @x ptr) value
{-# INLINE pokePtrOffset #-}

pokePtrArrayOffset ::
  forall x r a.
  (HasField x r a, Offset x r, Storable a) =>
  [a] -> ReaderT (Ptr r) IO ()
pokePtrArrayOffset values = do
  ptr <- ask
  liftIO $ foldForM_ (offset @x ptr) values \elemPtr value -> do
    poke elemPtr value
    return $ plusPtr elemPtr valueSize

  where
  valueSize = sizeOf (undefined :: a)
