module TestUtils.Parser where

import Parser.Parser
import Test.Hspec
import Parser.Error
import Control.Monad

shouldCompletelyParse :: (Eq a, Show a) => (Parser a, String) -> a -> Expectation
shouldCompletelyParse (p, s) r = parse p s `shouldBe` Right ("", r)

shouldPartiallyParse :: (Eq a, Show a) => (Parser a, String) -> (String, a) -> Expectation
shouldPartiallyParse (p, s) (r, v) = parse p s `shouldBe` Right (r, v)

shouldFailWithMsgAndCi :: (Eq a, Show a) => (Parser a, String) -> (String, String) -> Expectation
shouldFailWithMsgAndCi (p, s) (msg, ci) = parse p s `shouldBe` Left (ParseError {reason = msg, currentInput = ci})

passPartialFailSpec :: (Eq a, Show a) => String -> Parser a -> [(String, a)] -> [(String, a, String)] -> [(String, String, String)] -> SpecWith ()
passPartialFailSpec name parser pass partial fail =
    describe (name <> " pass/partial/fail") $ do
        forM_ pass $ \(input, expected) ->
            it ("resolves " <> show input <> " as " <> show expected) $ do
                (parser, input) `shouldCompletelyParse` expected

        forM_ partial $ \(input, expected, remainder) ->
            it ("resolves " <> show input <> " as " <> show expected <> " with remainder " <> show remainder) $ do
                (parser, input) `shouldPartiallyParse` (remainder, expected)

        forM_ fail $ \(input, reason, currentInput) ->
            it ("fails to parse " <> show input <> " for reason " <> show reason) $ do
                (parser, input) `shouldFailWithMsgAndCi` (reason, currentInput)
