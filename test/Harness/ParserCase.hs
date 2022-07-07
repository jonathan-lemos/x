module Harness.ParserCase where

import Data.Bifunctor
import Harness.TestCase
import Harness.TestDSLMonad
import Harness.With
import Test.Hspec
import X.Control.Parser
import X.Data.ParseError
import X.Utils.Function
import X.Utils.Functor

data ParserExpectation a = ParsedShouldEqual a | ParsedShouldSatisfy (a -> Bool) | ParseShouldFail String

data ParserCase a = ParserCase
    { pcInput :: String
    , pcExpectation :: ParserExpectation a
    , pcRemainder :: String
    , pcTitle :: String
    }

pcToTitleAndExpectation :: (Show a, Eq a) => ParserCase a -> Parser a -> (String, Expectation)
pcToTitleAndExpectation (ParserCase input expectation remainder title) parser =
    let parserResult = parse parser input
        exp =
            case expectation of
                ParsedShouldEqual expected -> parserResult `shouldBe` Right (remainder, expected)
                ParsedShouldSatisfy predicate ->
                    let newPredicate (Right (r, v)) = r == remainder && predicate v
                        newPredicate _ = False
                     in parserResult `shouldSatisfy` newPredicate
                ParseShouldFail reason -> parserResult `shouldBe` Left (ParseError reason remainder)
     in (title, exp)

type ParserCaseMonad a b = TestDSLMonad (ParserCase a) b

instance WithTitle (ParserCase a) where
    pc `withTitle` title = pc{pcTitle = title}

instance WithRemainder (ParserCase a) where
    pc `withRemainder` remainder = pc{pcRemainder = remainder}

-- | Defines a sequence of tests that all use the given parser. The String argument defines the name of the block of tests.
parserDesc :: (Show a, Eq a) => Parser a -> String -> ParserCaseMonad a b -> SpecWith ()
parserDesc parser title ps =
    describe title $ do
        tdmItems ps
            >$> pcToTitleAndExpectation
            >$> ($ parser)
            >$> uncurry it >$ sequence_

{- | The parser given in `parserDesc` should parse the **entire** input into the given value.
Use `withRemainder` if the parser should only parse some of the input.
-}
shouldParseTo :: (Show a, Eq a) => String -> a -> ParserCaseMonad a ()
shouldParseTo input expected =
    liftTdm $ ParserCase input (ParsedShouldEqual expected) (input <> " should parse to " <> show expected) ""

{- | The parser given in `parserDesc` should parse the **entire** input into a value that satisfies the given predicate.
Use `withRemainder` if the parser should only parse some of the input.
-}
shouldParseAndSatisfy :: String -> (a -> Bool) -> ParserCaseMonad a ()
shouldParseAndSatisfy input predicate =
    liftTdm $ ParserCase input (ParsedShouldSatisfy predicate) (input <> " should parse and satisfy predicate") ""

{- | The parser given in `parserDesc` should fail to parse the input with the given reason.
Must be followed by `andRemainder` to specify the location of the error within the input.
-}
shouldFailWithReason :: (Show a, Eq a) => String -> String -> String -> ParserCaseMonad a ()
shouldFailWithReason input reason remainder =
    liftTdm $ ParserCase input (ParseShouldFail reason) (input <> " should fail to parse with reason " <> show reason) ""

-- | Specifies the error location of `shouldFailWithReason`
andRemainder :: (String -> ParserCaseMonad a ()) -> String -> ParserCaseMonad a ()
andRemainder = ($)
