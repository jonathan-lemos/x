{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Harness.Typeclass.MonadCase where

import Harness.TestCase
import Harness.Typeclass.ApplicativeCase
import Harness.With
import Test.Hspec

{- | Verifies properties for the "happy path" monad e.g. Right and not Left, Just and not Nothing, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
-}
verifyMonadHappyPathUsageWithEqFn :: (Monad m, forall a. Show a => Show (m a)) => (forall a. a -> m a) -> String -> (forall a. Eq a => m a -> m a -> Bool) -> SpecWith ()
verifyMonadHappyPathUsageWithEqFn constructor name eq = do
    let a `shouldEqFn` b = a `should` eq b

    describe (name <> ": monad happy path") $ do
        desc (name <> ": return == constructor") $ do
            constructor 123 `shouldEqFn` return 123 `withTitle` (name <> " 123 == return 123")

{- | Verifies properties for the "happy path" monad e.g. Right and not Left, Just and not Nothing, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
-}
verifyMonadHappyPathUsage :: (Monad m, forall a. Show a => Show (m a), forall a. Eq a => Eq (m a)) => (forall a. a -> m a) -> String -> SpecWith ()
verifyMonadHappyPathUsage constructor name = verifyMonadHappyPathUsageWithEqFn constructor name (==)

{- | Verifies that a Monad conforms to the standard Monad laws, given that monad's data constructor, its name, and a function that verifies if two instances are equal.

The Monad must implement `Show` given that the underlying type also implements `Show`.
This function also verifies the Applicative and Functor laws.
-}
verifyMonadLawsWithEqFn :: (Monad m, forall a. Show a => Show (m a)) => (forall a. a -> m a) -> String -> (forall a. Eq a => m a -> m a -> Bool) -> SpecWith ()
verifyMonadLawsWithEqFn constructor name eq = do
    let a `shouldEqFn` b = a `should` eq b

    verifyApplicativeLawsWithEqFn constructor name eq

    describe (name <> ": monad laws") $ do
        desc (name <> ": return a >>= f == f a (left identity)") $ do
            (return 123 >>= constructor . show) `shouldEqFn` ((constructor . show) 123) `withTitle` ("return 123 >>= constructor . show == (constructor . show) 123")
            (return "abc" >>= constructor . length) `shouldEqFn` ((constructor . length) "abc") `withTitle` ("return \"abc\" >>= constructor . length == (constructor . length) \"abc\"")

        desc (name <> ": m >>= return == m (right identity)") $ do
            (constructor 123 >>= return) `shouldEqFn` constructor 123 `withTitle` (name <> " 123 >>= return == " <> name <> " 123")
            (constructor "abc" >>= return) `shouldEqFn` constructor "abc" `withTitle` (name <> " \"abc\" >>= return == " <> name <> " \"abc\"")

        desc (name <> ": (m >>= g) >>= h == m >>= (\\x -> g x >>= h)") $ do
            ((constructor 123 >>= constructor . show) >>= constructor . length)
                `shouldEqFn` (constructor 123 >>= \x -> (constructor . show) x >>= constructor . length)
                `withTitle` ("(" <> name <> " 123 >>= constructor . show) >>= constructor . length == " <> name <> " 123 >>= (\\x -> (constructor . show) x >>= constructor . length)")

{- | Verifies that a Monad instance conforms to the standard Monad laws, given that monad's data constructor and its name.

The Monad must implement `Show` and `Eq` given that the underlying type also implements `Show` and `Eq`.
This function also verifies the Applicative and Functor laws.
-}
verifyMonadLaws :: (Monad m, forall a. Show a => Show (m a), forall a. Eq a => Eq (m a)) => (forall a. a -> m a) -> String -> SpecWith ()
verifyMonadLaws constructor name = verifyMonadLawsWithEqFn constructor name (==)
