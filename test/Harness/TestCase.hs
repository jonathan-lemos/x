{-# LANGUAGE FlexibleInstances #-}

module Harness.TestCase where

import Test.Hspec
import X.Utils.Function
import X.Utils.Functor
import Harness.WithMessage

newtype TestCaseBundle a = TestCaseBundle [(String, Expectation)]

instance Functor TestCaseBundle where
    fmap _f (TestCaseBundle tcs) = TestCaseBundle tcs

instance Semigroup (TestCaseBundle a) where
    (TestCaseBundle as) <> (TestCaseBundle bs) = TestCaseBundle (as <> bs)

instance Monoid (TestCaseBundle a) where
    mempty = TestCaseBundle []

instance Applicative TestCaseBundle where
    pure _ = mempty
    (TestCaseBundle fs) <*> (TestCaseBundle as) = TestCaseBundle (fs <> as)

instance WithMessage (TestCaseBundle a) where
    (TestCaseBundle []) `withMessage` msg = TestCaseBundle []
    (TestCaseBundle [(_, exp)]) `withMessage` msg = TestCaseBundle [(msg, exp)]
    (TestCaseBundle (x:xs)) `withMessage` msg = TestCaseBundle [x] <> TestCaseBundle xs `withMessage` msg

desc :: String -> TestCaseBundle () -> SpecWith ()
desc title (TestCaseBundle tcs) = do
    describe title $ do
        tcs
            >$> uncurry it
            >$ sequence_

liftTcb :: String -> Expectation -> TestCaseBundle ()
liftTcb s e = TestCaseBundle [(s, e)]

should :: (Show a, Eq a) => a -> (a -> Bool) -> TestCaseBundle ()
a `should` f = liftTcb (show a <> " should satisfy predicate") (a `shouldSatisfy` f)

shouldEq :: (Show a, Eq a) => a -> a -> TestCaseBundle ()
a `shouldEq` b = liftTcb (show a <> " == " <> show b) (a `shouldBe` b)
