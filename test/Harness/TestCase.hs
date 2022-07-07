{-# LANGUAGE FlexibleInstances #-}

module Harness.TestCase where

import Test.Hspec
import X.Utils.Function
import X.Utils.Functor
import Harness.With
import Harness.TestDSLMonad
import Data.Data (tyConModule)
import Test.QuickCheck

newtype TestCase = TestCase { getTc :: (String, Expectation) }

type TestCaseMonad a = TestDSLMonad TestCase a

instance WithTitle TestCase where
    TestCase (_, exp) `withTitle` msg = TestCase (msg, exp)

desc :: String -> TestCaseMonad () -> SpecWith ()
desc title tcm = do
    describe title $ do
        tdmItems tcm
            >$> getTc
            >$> uncurry it
            >$ sequence_

liftTc :: String -> Expectation -> TestCaseMonad ()
liftTc s e = TestCase (s, e) >$ liftTdm

should :: (Show a, Eq a) => a -> (a -> Bool) -> TestCaseMonad ()
a `should` f = liftTc (show a <> " should satisfy predicate") (a `shouldSatisfy` f)

shouldEq :: (Show a, Eq a) => a -> a -> TestCaseMonad ()
a `shouldEq` b = liftTc (show a <> " == " <> show b) (a `shouldBe` b)
