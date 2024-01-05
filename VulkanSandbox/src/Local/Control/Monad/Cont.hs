module Local.Control.Monad.Cont (
  module Control.Monad.Cont,
  With,
  SomeWith,
  MonadWith,
  SomeMonadWith,
  IOWith,
  SomeIOWith,
  withContUncurried
) where

import Control.Monad.Cont

-- | A continuation-passing function.
type With a r = (a -> r) -> r
-- | A polymorphic continuation-passing function.
type SomeWith a = forall r. With a r
-- | A continuation-passing monadic function. The type contained within a `ContT r m a`.
type MonadWith a m r = With a (m r)
-- | A polymorphic continuation-passing monadic function.
type SomeMonadWith a m = forall r. MonadWith a m r
-- | A continuation-passing IO function. The type contained within a `ContT r IO a`.
type IOWith a r = MonadWith a IO r
-- | A polymorphic continuation-passing IO function.
type SomeIOWith a = forall r. IOWith a r

withContUncurried :: ((a -> b -> r) -> r) -> With (a, b) r
withContUncurried with2 cont = with2 $ \a b -> cont (a, b)
