{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Harness.Typeclass.MonadCase where

import Harness.TestCase
import Harness.Typeclass.ApplicativeCase
import Harness.With
import Test.Hspec

{- | Verifies that a Monad instance is valid, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
This function also verifies that the Monad's Applicative and Functor instance.
-}
monadDescFn :: (Monad m, forall a. Show a => Show (m a)) => (forall a. a -> m a) -> String -> (forall a. Eq a => m a -> m a -> Bool) -> SpecWith ()
monadDescFn constructor name eq = do
    let a `shouldEqFn` b = a `should` eq b

    applicativeDescFn constructor name eq

    desc (name <> ": return == constructor") $ do
        constructor 123 `shouldEqFn` return 123 `withTitle` (name <> " 123 == return 123")

    desc (name <> ": return a >>= f == f a (left identity)") $ do
        (return 123 >>= constructor . show) `shouldEqFn` ((constructor. show) 123) `withTitle` ("return 123 >>= constructor . show == (constructor . show) 123")
        (return "abc" >>= constructor . length) `shouldEqFn` ((constructor . length) "abc") `withTitle` ("return \"abc\" >>= constructor . length == (constructor . length) \"abc\"")

    desc (name <> ": m >>= return == m (right identity)") $ do
        (constructor 123 >>= return) `shouldEqFn` constructor 123 `withTitle` (name <> " 123 >>= return == " <> name <> " 123")
        (constructor "abc" >>= return) `shouldEqFn` constructor "abc" `withTitle` (name <> " \"abc\" >>= return == " <> name <> " \"abc\"")

    desc (name <> ": (m >>= g) >>= h == m >>= (\\x -> g x >>= h)") $ do
        ((constructor 123 >>= constructor . show) >>= constructor . length)
            `shouldEqFn`
            (constructor 123 >>= \x -> (constructor . show) x >>= constructor . length)
            `withTitle` ("(" <> name <> " 123 >>= constructor . show) >>= constructor . length == " <> name <> " 123 >>= (\\x -> (constructor . show) x >>= constructor . length)" )

{- | Verifies that a Monad instance is valid, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
This function also verifies that the Monad's Applicative and Functor instance.
-}
monadDesc :: (Monad m, forall a. Show a => Show (m a), forall a. Eq a => Eq (m a)) => (forall a. a -> m a) -> String -> SpecWith ()
monadDesc constructor name = monadDescFn constructor name (==)
