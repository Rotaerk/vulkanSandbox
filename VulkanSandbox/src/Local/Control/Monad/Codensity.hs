module Local.Control.Monad.Codensity (
  module Control.Monad.Codensity,
  With,
  SomeWith,
  MWith,
  SomeMWith,
  IOWith,
  SomeIOWith,
  withUncurriedCont,
  beforeCont
) where

import Control.Monad.Codensity

-- |A continuation-passing function. Equivalent to the type contained within a `Cont r a`.
type With a r = (a -> r) -> r

-- |A polymorphic continuation-passing function. Equivalent to the type contained within a `Codensity Identity a`.
type SomeWith a = forall r. With a r

-- |A continuation-passing monadic function. The type contained within a `ContT r m a`.
type MWith a m r = With a (m r)

-- |A polymorphic continuation-passing monadic function. The type contained within a `Codensity m a`.
type SomeMWith a m = forall r. MWith a m r

-- |A continuation-passing IO function. The type contained within a `ContT r IO a`.
type IOWith a r = MWith a IO r

-- |A polymorphic continuation-passing IO function. The type contained within a `Codensity IO a`
type SomeIOWith a = forall r. IOWith a r

withUncurriedCont :: ((a -> b -> r) -> r) -> With (a, b) r
withUncurriedCont with2 cont = with2 \a b -> cont (a, b)

beforeCont :: Monad m => (a -> m ()) -> SomeMWith a m -> SomeMWith a m
beforeCont action with cont = with \a -> action a >> cont a
