{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Harness.Typeclass.FunctorCase where

import Harness.TestCase
import Harness.With
import Test.Hspec

{- | Verifies properties for the "happy path" functor e.g. Right and not Left, Just and not Nothing, etc.

The Functor must implement `Show` given that the underlying type also implements `Show`.
-}
verifyFunctorHappyPathWithEqFn :: (Functor f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> SpecWith ()
verifyFunctorHappyPathWithEqFn constructor name eq = do
    let a `shouldEqFn` b = a `should` eq b

    describe (name <> ": functor happy path") $ do
        desc (name <> ": basic fmap tests") $ do
            fmap length (constructor "abc") `shouldEqFn` constructor (length "abc") `withTitle` ("length <$> " <> name <> " \"abc\" == " <> name <> " 3")
            fmap show (constructor 123) `shouldEqFn` constructor (show 123) `withTitle` ("show <$> " <> name <> " 123 == " <> name <> " \"123\"")

{- | Verifies properties for the "happy path" functor e.g. Right and not Left, Just and not Nothing, given that functor's data constructor and its name.

The Functor must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
-}
verifyFunctorHappyPath :: (Functor f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> SpecWith ()
verifyFunctorHappyPath constructor name = verifyFunctorHappyPathWithEqFn constructor name (==)

{- | Verifies that a Functor instance is valid, given that functor's data constructor, its name, and a function that verifies if two instances are equal.

The Functor must implement `Show` given that the underlying type also implements `Show`.
-}
verifyFunctorLawsWithEqFn :: (Functor f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> SpecWith ()
verifyFunctorLawsWithEqFn constructor name eq = do
    let a `shouldEqFn` b = a `should` eq b

    describe (name <> ": functor laws") $ do
        desc (name <> ": fmap id == id (identity)") $ do
            fmap id (constructor "abc") `shouldEqFn` constructor "abc" `withTitle` ("fmap id " <> name <> " \"abc\" == " <> name <> " \"abc\"")
            fmap id (constructor 3) `shouldEqFn` constructor 3 `withTitle` ("fmap id " <> name <> " 3 == " <> name <> " 3")

        desc (name <> ": fmap (f . g) == fmap f . fmap g (composition)") $ do
            fmap (show . length) (constructor "abc") `shouldEqFn` (fmap show . fmap length) (constructor "abc") `withTitle` ("fmap (show . length) " <> name <> " \"abc\" == " <> "(fmap show . fmap length) " <> name <> " \"abc\"")
            fmap (length . show) (constructor 3) `shouldEqFn` (fmap length . fmap show) (constructor 3) `withTitle` ("fmap (length . show) " <> name <> " 3 == " <> "(fmap length . fmap show) " <> name <> " 3")

{- | Verifies that a Functor instance is valid, given that functor's data constructor and its name.

The Functor must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
-}
verifyFunctorLaws :: (Functor f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> SpecWith ()
verifyFunctorLaws constructor name = verifyFunctorLawsWithEqFn constructor name (==)
