module Local.Control.Monad (
  module Control.Monad,
  foldForM_
) where

import Control.Monad

foldForM_ :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m ()
foldForM_ a xs f = foldM_ f a xs
