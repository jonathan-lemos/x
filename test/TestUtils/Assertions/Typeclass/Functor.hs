{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module TestUtils.Assertions.Typeclass.Functor where

import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion
import TestUtils.Collector

{- | Verifies properties for the "happy path" functor e.g. Right and not Left, Just and not Nothing, etc.

The Functor must implement `Show` given that the underlying type also implements `Show`.
-}
verifyFunctorHappyPathWithEqFn :: (Functor f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> Assertion
verifyFunctorHappyPathWithEqFn constructor name eq =
    basicAssertion $ do
        assert (fmap length (constructor "abc") `eq` constructor (length "abc")) `withTitle` ("length <$> " <> name <> " \"abc\" == " <> name <> " 3")
        assert (fmap show (constructor (123 :: Int)) `eq` constructor (show (123 :: Int))) `withTitle` ("show <$> " <> name <> " 123 == " <> name <> " \"123\"")

{- | Verifies properties for the "happy path" functor e.g. Right and not Left, Just and not Nothing, given that functor's data constructor and its name.

The Functor must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
-}
verifyFunctorHappyPath :: (Functor f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> Assertion
verifyFunctorHappyPath constructor name = verifyFunctorHappyPathWithEqFn constructor name (==)

{- | Verifies that a Functor instance is valid, given that functor's data constructor, its name, and a function that verifies if two instances are equal.

The Functor must implement `Show` given that the underlying type also implements `Show`.
-}
verifyFunctorLawsWithEqFn :: (Functor f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> Assertion
verifyFunctorLawsWithEqFn constructor name eq =
    basicAssertion $ do
        assert (fmap id (constructor "abc") `eq` constructor "abc") `withTitle` ("fmap id " <> name <> " \"abc\" == " <> name <> " \"abc\"")
        assert (fmap id (constructor (3 :: Int)) `eq` constructor (3 :: Int)) `withTitle` ("fmap id " <> name <> " 3 == " <> name <> " 3")

        assert (fmap (show . length) (constructor "abc") `eq` (fmap show . fmap length) (constructor "abc")) `withTitle` ("fmap (show . length) " <> name <> " \"abc\" == " <> "(fmap show . fmap length) " <> name <> " \"abc\"")
        assert (fmap (length . show) (constructor (3 :: Int)) `eq` (fmap length . fmap show) (constructor (3 :: Int))) `withTitle` ("fmap (length . show) " <> name <> " 3 == " <> "(fmap length . fmap show) " <> name <> " 3")

{- | Verifies that a Functor instance is valid, given that functor's data constructor and its name.

The Functor must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
-}
verifyFunctorLaws :: (Functor f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> Assertion
verifyFunctorLaws constructor name = verifyFunctorLawsWithEqFn constructor name (==)
