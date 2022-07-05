module Harness.ParserCase where

import Data.Bifunctor
import Harness.TestCase
import Harness.TestDSLMonad
import Harness.WithMessage
import Test.Hspec
import X.Control.Parser
import X.Data.ParseError
import X.Utils.Function
import X.Utils.Functor

newtype ParserCase a = ParserCase {parserCaseFn :: Parser a -> (String, Expectation)}

liftPc :: (Parser a -> (String, Expectation)) -> ParserCaseMonad a ()
liftPc = ParserCase |> liftTdm

type ParserCaseMonad a b = TestDSLMonad (ParserCase a) b

instance WithMessage (ParserCase a) where
    (ParserCase f) `withMessage` msg = ParserCase $ f |> first (const msg)

parserDesc :: Parser a -> String -> ParserCaseMonad a b -> SpecWith ()
parserDesc parser title ps =
    describe title $ do
        tdmItems ps
            >$> parserCaseFn
            >$> ($ parser)
            >$> uncurry it >$ sequence_

shouldTotallyParseAndSatisfy :: (Show a, Eq a) => String -> (a -> Bool) -> ParserCaseMonad a ()
shouldTotallyParseAndSatisfy input predicate =
    let parserPredicate (Right ("", v)) = predicate v
        parserPredicate _ = False
     in liftPc $ \parser -> (input <> " should parse and satisfy predicate", parse parser input `shouldSatisfy` parserPredicate)

shouldTotallyParseTo :: (Show a, Eq a) => String -> a -> ParserCaseMonad a ()
shouldTotallyParseTo input expected =
    liftPc $ \parser -> (input <> " should parse to " <> show expected, parse parser input `shouldBe` Right ("", expected))

shouldPartiallyParseTo :: (Show a, Eq a) => String -> a -> String -> ParserCaseMonad a ()
shouldPartiallyParseTo input expected remainder =
    liftPc $ \parser -> (input <> " should parse to " <> show expected, parse parser input `shouldBe` Right ("", expected))

withRemainder :: (String -> a) -> String -> a
withRemainder = ($)

shouldFailWith :: (Show a, Eq a) => String -> ParseError -> ParserCaseMonad a ()
shouldFailWith input parseError =
    liftPc $ \parser -> (input <> " should fail to parse with reason " <> show (reason parseError) <> " and input " <> show (currentInput parseError), parse parser input `shouldEq` Left parseError)
