module Local.Control.Monad.Cont (
  module Control.Monad.Cont,
  CPS,
  MonadCPS,
  IOCPS,
  SomeIOCPS,
  uncurryCPS2
) where

import Control.Monad.Cont

-- | A continuation-passing style function.
type CPS r a = (a -> r) -> r
-- | A continuation-passing style monadic function. The type contained within a `ContT r m a`.
type MonadCPS r m a = CPS (m r) a
-- | A continuation-passing style IO function. The type contained within a `ContT r IO a`.
type IOCPS r a = MonadCPS r IO a
-- | A polymorphic continuation-passing style IO function.
type SomeIOCPS a = forall r. IOCPS r a

uncurryCPS2 :: ((a -> b -> r) -> r) -> CPS r (a, b)
uncurryCPS2 cps2 cont = cps2 $ \a b -> cont (a, b)
