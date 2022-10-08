{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module TestUtils.Assertions.Typeclass.Monad where

import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion
import TestUtils.Assertions.Typeclass.Applicative
import TestUtils.Collector

{- | Verifies properties for the "happy path" monad e.g. Right and not Left, Just and not Nothing, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
-}
verifyMonadHappyPathUsageWithEqFn :: (Monad m, forall a. Show a => Show (m a)) => (forall a. a -> m a) -> String -> (forall a. Eq a => m a -> m a -> Bool) -> Assertion
verifyMonadHappyPathUsageWithEqFn constructor name eq = basicAssertion $ do
    assert (constructor (123 :: Int) `eq` return 123) `withTitle` (name <> " 123 == return 123")

{- | Verifies properties for the "happy path" monad e.g. Right and not Left, Just and not Nothing, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
-}
verifyMonadHappyPathUsage :: (Monad m, forall a. Show a => Show (m a), forall a. Eq a => Eq (m a)) => (forall a. a -> m a) -> String -> Assertion
verifyMonadHappyPathUsage constructor name = verifyMonadHappyPathUsageWithEqFn constructor name (==)

{- | Verifies that a Monad conforms to the standard Monad laws, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
This function also verifies the Applicative and Functor laws.
-}
verifyMonadLawsWithEqFn :: (Monad m, forall a. Show a => Show (m a)) => (forall a. a -> m a) -> String -> (forall a. Eq a => m a -> m a -> Bool) -> Assertion
verifyMonadLawsWithEqFn constructor name eq = do
    verifyApplicativeLawsWithEqFn constructor name eq

    basicAssertion $ do
        assert ((return (123 :: Int) >>= constructor . show) `eq` ((constructor . show) (123 :: Int))) `withTitle` ("return 123 >>= constructor . show == (constructor . show) 123")
        assert ((return "abc" >>= constructor . length) `eq` ((constructor . length) "abc")) `withTitle` ("return \"abc\" >>= constructor . length == (constructor . length) \"abc\"")

        assert ((constructor (123 :: Int) >>= return) `eq` constructor 123) `withTitle` (name <> " 123 >>= return == " <> name <> " 123")
        assert ((constructor "abc" >>= return) `eq` constructor "abc") `withTitle` (name <> " \"abc\" >>= return == " <> name <> " \"abc\"")

        assert
            ( ((constructor (123 :: Int) >>= constructor . show) >>= constructor . length)
                `eq` (constructor (123 :: Int) >>= \x -> (constructor . show) x >>= constructor . length)
            )
            `withTitle` ("(" <> name <> " 123 >>= constructor . show) >>= constructor . length == " <> name <> " 123 >>= (\\x -> (constructor . show) x >>= constructor . length)")

{- | Verifies that a Monad instance conforms to the standard Monad laws, given that monad's data constructor and its name.

The Monad must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
This function also verifies the Applicative and Functor laws.
-}
verifyMonadLaws :: (Monad m, forall a. Show a => Show (m a), forall a. Eq a => Eq (m a)) => (forall a. a -> m a) -> String -> Assertion
verifyMonadLaws constructor name = verifyMonadLawsWithEqFn constructor name (==)
