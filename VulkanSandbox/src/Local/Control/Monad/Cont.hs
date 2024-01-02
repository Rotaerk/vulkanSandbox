module Local.Control.Monad.Cont (
  module Control.Monad.Cont,
  With,
  AnyWith,
  IOWith,
  AnyIOWith,
  uncurryWith2
) where

import Control.Monad.Cont

type With a r = (a -> r) -> r
type AnyWith a = forall r. With a r
type IOWith a r = With a (IO r)
type AnyIOWith a = forall r. IOWith a r

uncurryWith2 :: ((a -> b -> r) -> r) -> With (a, b) r
uncurryWith2 with2 cont = with2 $ \a b -> cont (a, b)
