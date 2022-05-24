module TestUtils.ArithmeticExpression where

import Data.Number.CReal
import State.XState
import Types.Evaluatable.Evaluatable
import Parser.Parser
import Parser.Error
import Test.Hspec
import Data.Bifunctor

mkState :: [(String, CReal)] -> XState
mkState = foldr (\(var, value) state -> putVar state var value) newState

eval :: (Evaluatable e) => XState -> Parser e -> (String -> Either ParseError (String, Either String CReal))
eval s = parse . fmap (`evaluate` s)

shouldEvaluateToWithState :: (Evaluatable e) => XState -> (Parser e, String) -> CReal -> Expectation
shouldEvaluateToWithState state (parser, input) answer =
    second (`evaluate` state) <$> parse parser input `shouldBe` Right ("", Right answer)
