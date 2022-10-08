{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module TestUtils.Assertions.Typeclass.Applicative where

import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion
import TestUtils.Assertions.Typeclass.Functor
import TestUtils.Collector

{- | Verifies properties for the "happy path" applicative e.g. Right and not Left, Just and not Nothing, etc.

The Applicative must implement `Show` given that the underlying type also implements `Show`.
-}
verifyApplicativeHappyPathWithEqFn :: (Applicative f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> Assertion
verifyApplicativeHappyPathWithEqFn constructor name eq = basicAssertion $ do
    assert (((-) <$> constructor (9 :: Int) <*> constructor 3) `eq` constructor 6) `withTitle` (name <> " (-) <$> " <> name <> " 9 <*> " <> name <> " 3 == " <> name <> " 6")
    assert (((<>) <$> constructor "foo" <*> constructor "bar") `eq` constructor "foobar") `withTitle` (name <> " (<>) <$> " <> name <> " \"foo\" <*> " <> name <> " \"bar\" == " <> name <> " \"foobar\"")
    assert (constructor (123 :: Int) `eq` pure 123) `withTitle` (name <> " 123 == pure 123")
    assert (constructor "abc" `eq` pure "abc") `withTitle` (name <> " \"abc\" == pure \"abc\"")

{- | Verifies properties for the "happy path" applicative e.g. Right and not Left, Just and not Nothing, given that applicative's data constructor and its name.

The Applicative must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
-}
verifyApplicativeHappyPath :: (Applicative f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> Assertion
verifyApplicativeHappyPath constructor name = verifyApplicativeHappyPathWithEqFn constructor name (==)

{- | Verifies that an Applicative conforms to the standard Applicative laws, given that applicative's data constructor, its name, and a function that verifies if two instances are equal.

The Applicative must implement `Show` given that the underlying type also implements `Show`.
This function also verifies the Functor laws.
-}
verifyApplicativeLawsWithEqFn :: (Applicative f, forall a. Show a => Show (f a)) => (forall a. a -> f a) -> String -> (forall a. Eq a => f a -> f a -> Bool) -> Assertion
verifyApplicativeLawsWithEqFn constructor name eq = do
    verifyFunctorLawsWithEqFn constructor name eq
    basicAssertion $ do
        assert ((pure id <*> constructor (123 :: Int)) `eq` constructor (123 :: Int)) `withTitle` ("pure id <*> " <> name <> " 123 == " <> name <> " 123")
        assert ((pure id <*> constructor "abc") `eq` constructor "abc") `withTitle` ("pure id <*> " <> name <> " \"abc\" == " <> name <> " \"abc\"")

        assert ((pure show <*> pure (123 :: Int)) `eq` pure (show (123 :: Int))) `withTitle` ("pure show <*> pure 123 == pure (show 123)")
        assert ((pure length <*> pure "abc") `eq` pure (length "abc")) `withTitle` ("pure length <*> pure \"abc\" == pure (show \"abc\")")

        assert ((pure show <*> pure (123 :: Int)) `eq` (pure ($ (123 :: Int)) <*> pure show)) `withTitle` ("pure show <*> pure 123 == pure ($ 123) <*> pure show")
        assert ((pure show <*> pure "abc") `eq` (pure ($ "abc") <*> pure show)) `withTitle` ("pure show <*> pure \"abc\" == pure ($ \"abc\") <*> pure show")

        assert ((pure (.) <*> pure length <*> pure show <*> pure (123 :: Int)) `eq` (pure length <*> (pure show <*> pure (123 :: Int)))) `withTitle` ("pure (.) <*> pure length <*> pure show <*> pure 123 == pure length <*> (pure show <*> pure 123)")

{- | Verifies that an Applicative conforms to the standard Applicative laws, given that applicative's data constructor and its name.

The Applicative must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
This function also verifies the Applicative's Functor instance.
-}
verifyApplicativeLaws :: (Applicative f, forall a. Show a => Show (f a), forall a. Eq a => Eq (f a)) => (forall a. a -> f a) -> String -> Assertion
verifyApplicativeLaws constructor title = verifyApplicativeLawsWithEqFn constructor title (==)
