{-# LANGUAGE BlockArguments #-}

module Local.Prelude (
  withContUncurried
) where

withContUncurried :: ((a -> b -> r) -> r) -> ((a, b) -> r) -> r
withContUncurried with2 cont = with2 \a b -> cont (a, b)
