module Harness.ParserCaseBundle where

import Data.Bifunctor
import Harness.TestCase
import Harness.WithMessage
import Test.Hspec
import X.Control.Parser
import X.Data.ParseError
import X.Utils.Function
import X.Utils.Functor

newtype ParserCaseBundle a b = ParserCaseBundle [Parser a -> (String, Expectation)]

liftPc :: (Parser a -> (String, Expectation)) -> ParserCaseBundle a b
liftPc = (: []) |> ParserCaseBundle

instance Functor (ParserCaseBundle a) where
    fmap _f (ParserCaseBundle ps) = ParserCaseBundle ps

instance Applicative (ParserCaseBundle a) where
    pure _ = mempty
    (ParserCaseBundle f) <*> (ParserCaseBundle a) = ParserCaseBundle (f <> a)

instance WithMessage (ParserCaseBundle a b) where
    (ParserCaseBundle []) `withMessage` msg = ParserCaseBundle []
    (ParserCaseBundle [f]) `withMessage` msg = ParserCaseBundle [f |> first (const msg)]
    (ParserCaseBundle (x : xs)) `withMessage` msg = ParserCaseBundle [x] <> ParserCaseBundle xs `withMessage` msg

instance Semigroup (ParserCaseBundle a b) where
    (ParserCaseBundle as) <> (ParserCaseBundle bs) = ParserCaseBundle (as <> bs)

instance Monoid (ParserCaseBundle a b) where
    mempty = ParserCaseBundle []

parserDesc :: Parser a -> String -> ParserCaseBundle a () -> SpecWith ()
parserDesc parser title (ParserCaseBundle ps) =
    describe title $ do
        ps >$> ($ parser) >$> uncurry it >$ sequence_

shouldTotallyParseTo :: (Show a, Eq a) => String -> a -> ParserCaseBundle a ()
shouldTotallyParseTo input expected =
    liftPc $ \parser -> (input <> " should parse to " <> show expected, parse parser input `shouldBe` Right ("", expected))

shouldPartiallyParseTo :: (Show a, Eq a) => String -> (a, String) -> ParserCaseBundle a ()
shouldPartiallyParseTo input (expected, remainder) =
    liftPc $ \parser -> (input <> " should parse to " <> show expected, parse parser input `shouldBe` Right ("", expected))

shouldFailWith :: (Show a, Eq a) => String -> ParseError -> ParserCaseBundle a ()
shouldFailWith input parseError =
    liftPc $ \parser -> (input <> " should fail to parse with reason " <> show (reason parseError) <> " and input " <> show (currentInput parseError), parse parser input `shouldEq` Left parseError)
