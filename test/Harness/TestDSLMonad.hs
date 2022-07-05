module Harness.TestDSLMonad where

import Harness.WithMessage

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

instance WithMessage a => WithMessage (TestDSLMonad a b) where
    (TestDSLMonad [] a) `withMessage` msg = TestDSLMonad [] a
    (TestDSLMonad [tail] a) `withMessage` msg = TestDSLMonad [tail `withMessage` msg] a
    (TestDSLMonad (x:xs) a) `withMessage` msg = TestDSLMonad [x] a <> TestDSLMonad xs a `withMessage` msg

liftTdm :: a -> TestDSLMonad a ()
liftTdm a = TestDSLMonad [a] ()

tdmItems :: TestDSLMonad a b -> [a]
tdmItems (TestDSLMonad as _) = as
