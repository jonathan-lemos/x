module X.TestUtils.Parser where

import Control.Monad
import Data.Bifunctor
import Test.Hspec
import X.Control.Parser
import X.Data.ParseError
import X.TestUtils.Either
import X.Utils.Function
import X.Utils.Functor

shouldCompletelyParse :: (Eq a, Show a) => (Parser a, String) -> a -> Expectation
shouldCompletelyParse (p, s) r = parse p s `shouldBe` Right ("", r)

shouldPartiallyParse :: (Eq a, Show a) => (Parser a, String) -> (String, a) -> Expectation
shouldPartiallyParse (p, s) (r, v) = parse p s `shouldBe` Right (r, v)

shouldFailWithMsgAndCi :: (Eq a, Show a) => (Parser a, String) -> (String, String) -> Expectation
shouldFailWithMsgAndCi (p, s) (msg, ci) = parse p s `shouldBe` Left (ParseError{reason = msg, currentInput = ci})

parserSpec :: (Eq a, Show a) => Parser a -> String -> [(String, Either ParseError (String, a) -> Bool, String)] -> SpecWith ()
parserSpec parser title testCases =
    describe title $ do
        forM_ testCases $ \(input, predicate, tcTitle) ->
            it tcTitle $ do
                parse parser input `shouldSatisfy` predicate

successfulParseFnSpec :: (Eq a, Show a) => Parser a -> String -> [(String, a -> Bool, String -> Bool, String)] -> SpecWith ()
successfulParseFnSpec title parser cases =
    parserSpec title parser $
        cases >$> \(input, predicate, remainderShould, tcName) ->
            (input, rightIs $ \(remainder, value) -> predicate value && remainderShould remainder, tcName)

totalParseFnSpec :: (Eq a, Show a) => Parser a -> String -> [(String, a -> Bool)] -> SpecWith ()
totalParseFnSpec title parser cases =
    successfulParseFnSpec title parser $
        cases >$> \(input, predicate) -> (input, predicate, (== ""), "resolves " <> input)

partialParseFnSpec :: (Eq a, Show a) => Parser a -> String -> [(String, a -> Bool, String)] -> SpecWith ()
partialParseFnSpec parser title cases =
    successfulParseFnSpec parser title $
        cases >$> \(input, predicate, remainder) -> (input, predicate, (== remainder), "resolves " <> input <> " with remainder " <> remainder)

failParseFnSpec :: (Eq a, Show a) => Parser a -> String -> [(String, ParseError -> Bool)] -> SpecWith ()
failParseFnSpec title parser cases =
    parserSpec title parser $
        cases >$> \(input, predicate) -> (input, leftIs predicate, "fails on " <> input)

totalParseSpec :: (Eq a, Show a) => Parser a -> String -> [(String, a)] -> SpecWith ()
totalParseSpec parser title cases =
    totalParseFnSpec parser title $ cases >$> second (==)

partialParseSpec :: (Eq a, Show a) => Parser a -> String -> [(String, a, String)] -> SpecWith ()
partialParseSpec parser title cases =
    partialParseFnSpec parser title $ cases >$> \(input, value, remainder) -> (input, (==) value, remainder)

failParseSpec :: (Eq a, Show a) => Parser a -> String -> [(String, ParseError)] -> SpecWith ()
failParseSpec parser title cases =
    partialParseFnSpec parser title $ cases >$> \(input, value, remainder) -> (input, (==) value, remainder)
