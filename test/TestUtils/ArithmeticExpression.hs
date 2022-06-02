module TestUtils.ArithmeticExpression where

import Data.Number.CReal
import State.XState
import Parser.Parser
import Parser.Error
import Test.Hspec
import Data.Bifunctor
import Evaluation.ToValue
import State.Value
import Unit.Unit

eval :: (ToValue e) => XState -> Parser e -> (String -> Either ParseError (String, Either String Value))
eval s = parse . fmap (`toValue` s)

shouldEvaluateToWithState :: (ToValue e) => XState -> (Parser e, String) -> Value -> Expectation
shouldEvaluateToWithState state (parser, input) answer =
    second (`toValue` state) <$> parse parser input `shouldBe` Right ("", Right answer)

shouldEvaluateToUnitlessWithState :: (ToValue e) => XState -> (Parser e, String) -> CReal -> Expectation
shouldEvaluateToUnitlessWithState state pi answer = shouldEvaluateToWithState state pi (Numeric answer Nothing)

shouldEvaluateToUnitQuantityWithState :: (ToValue e) => XState -> (Parser e, String) -> CReal -> Unit -> Expectation
shouldEvaluateToUnitQuantityWithState state pi answer unit = shouldEvaluateToWithState state pi (Numeric answer (Just unit))
