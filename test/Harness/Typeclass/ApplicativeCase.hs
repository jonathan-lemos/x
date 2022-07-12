{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Harness.Typeclass.ApplicativeCase where

import Harness.TestCase
import Harness.Typeclass.FunctorCase
import Harness.With
import Test.Hspec
import Control.Applicative

{- | Verifies that a Applicative instance is valid, given that applicative's data constructor, its name, and a function that verifies if two instances are equal.

The Applicative must implement `Show` given that the underlying type also implements `Show`.
This function also verifies the Applicative's Functor instance.
-}
applicativeDescFn :: (Applicative f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> SpecWith ()
applicativeDescFn constructor name eq = do
    let a `shouldEqFn` b = a `should` eq b

    functorDescFn constructor name eq

    desc (name <> ": usage tests") $ do
        ((-) <$> constructor 9 <*> constructor 3) `shouldEqFn` constructor 6 `withTitle` (name <> " (-) <$> " <> name <> " 9 <*> " <> name <> " 3 == " <> name <> " 6")
        ((<>) <$> constructor "foo" <*> constructor "bar") `shouldEqFn` constructor "foobar" `withTitle` (name <> " (<>) <$> " <> name <> " \"foo\" <*> " <> name <> " \"bar\" == " <> name <> " \"foobar\"")

    desc (name <> ": == pure") $ do
        constructor 123 `shouldEqFn` pure 123 `withTitle` (name <> " 123 == pure 123")
        constructor "abc" `shouldEqFn` pure "abc" `withTitle` (name <> " \"abc\" == pure \"abc\"")

    desc (name <> ": pure id <*> x == x (identity)") $ do
        pure id <*> constructor 123 `shouldEqFn` constructor 123 `withTitle` ("pure id <*> " <> name <> " 123 == " <> name <> " 123")
        pure id <*> constructor "abc" `shouldEqFn` constructor "abc" `withTitle` ("pure id <*> " <> name <> " \"abc\" == " <> name <> " \"abc\"")

    desc (name <> ": pure f <*> pure x = pure (f x) (homomorphism)") $ do
        (pure show <*> pure 123) `shouldEqFn` pure (show 123) `withTitle` ("pure show <*> pure 123 == pure (show 123)")
        (pure length <*> pure "abc") `shouldEqFn` pure (length "abc") `withTitle` ("pure length <*> pure \"abc\" == pure (show \"abc\")")

    desc (name <> ": u <*> pure y = pure ($ y) <*> u (interchange)") $ do
        (pure show <*> pure 123) `shouldEqFn` (pure ($ 123) <*> pure show) `withTitle` ("pure show <*> pure 123 == pure ($ 123) <*> pure show")
        (pure show <*> pure "abc") `shouldEqFn` (pure ($ "abc") <*> pure show) `withTitle` ("pure show <*> pure \"abc\" == pure ($ \"abc\") <*> pure show")

    desc (name <> ": pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (composition)") $ do
        (pure (.) <*> pure length <*> pure show <*> pure 123) `shouldEqFn` (pure length <*> (pure show <*> pure 123)) `withTitle` ("pure (.) <*> pure length <*> pure show <*> pure 123 == pure length <*> (pure show <*> pure 123)")

{- | Verifies that a Applicative instance is valid, given that applicative's data constructor and its name.

The Applicative must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
This function also verifies the Applicative's Functor instance.
-}
applicativeDesc :: (Applicative f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> SpecWith ()
applicativeDesc constructor title = applicativeDescFn constructor title (==)
