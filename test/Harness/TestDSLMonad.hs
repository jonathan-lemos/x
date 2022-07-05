module Harness.TestDSLMonad where

import Harness.With

{- | A Monad whose primary purpose is to collect `a`'s into a list.
Its type argument `b` is mostly unused, but the monad contains a single value required for the type signature of `>>=`.
-}
data TestDSLMonad a b = TestDSLMonad [a] b

instance Functor (TestDSLMonad a) where
    fmap f (TestDSLMonad as b) = TestDSLMonad as (f b)

instance Applicative (TestDSLMonad a) where
    pure = TestDSLMonad []
    (TestDSLMonad as f) <*> (TestDSLMonad bs b) = TestDSLMonad (as <> bs) (f b)

instance Monad (TestDSLMonad a) where
    (TestDSLMonad as a) >>= f =
        let TestDSLMonad bs b = f a
         in TestDSLMonad (as <> bs) b

instance Semigroup (TestDSLMonad a b) where
    (TestDSLMonad as a) <> (TestDSLMonad bs b) = TestDSLMonad (as <> bs) b

instance Monoid b => Monoid (TestDSLMonad a b) where
    mempty = TestDSLMonad [] mempty

modifyLast :: (a -> a) -> [a] -> [a]
modifyLast _ [] = []
modifyLast f [a] = [f a]
modifyLast f (x : xs) = x : modifyLast f xs

instance WithRemainder a => WithRemainder (TestDSLMonad a b) where
    (TestDSLMonad as a) `withRemainder` remainder = TestDSLMonad (modifyLast (`withRemainder` remainder) as) a

instance WithTitle a => WithTitle (TestDSLMonad a b) where
    (TestDSLMonad as a) `withTitle` title = TestDSLMonad (modifyLast (`withTitle` title) as) a

-- | Converts a value into the equivalent TestDSLMonad
liftTdm :: a -> TestDSLMonad a ()
liftTdm a = TestDSLMonad [a] ()

-- | Gets the collected values from a TestDSLMonad
tdmItems :: TestDSLMonad a b -> [a]
tdmItems (TestDSLMonad as _) = as
