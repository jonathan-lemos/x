module Parser.Parsers.AST.ArithmeticExpressionSpec where

import Data.Bifunctor
import Data.Number.CReal
import Evaluation.ToValue
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import State.XState
import Test.Hspec
import TestUtils.ArithmeticExpression
import TestUtils.Parser
import TestUtils.State
import Types.AST.ArithmeticExpression
import Types.AST.Token.Scalar
import Types.AST.UnitExpression
import State.Value
import Unit.Unit

isParentheses :: Factor -> Bool
isParentheses (Parentheses _) = True
isParentheses _ = False

spec :: Spec
spec = do
    passPartialFailFnSpec
        "factor"
        factor
        [ ("2", (== FactorScalar (Number 2)))
        , ("(2)", isParentheses)
        , ("( 2 )", isParentheses)
        , ("(2 + 2)", isParentheses)
        , ("a", (== FactorScalar (Variable "a")))
        , ("foobar", (== FactorScalar (Variable "foobar")))
        ]
        [ ("2 || 5", (== FactorScalar (Number 2)), " || 5")
        , ("(2) || 5", isParentheses, " || 5")
        ]
        [("@", "Expected a number, variable, or ( expression )", "@")]

    let state = mkState [("a", Numeric 4 Nothing), ("foo", Numeric 9 (Just $ BaseUnit "kg"))]
    let ae = (`toValue` state) <$> arithmeticExpression

    let evaluatesTo n (Right r) = n == r
        evaluatesTo _ _ = False

    let evaluatesToScalar n = evaluatesTo (Numeric n Nothing)

    let evaluatesToUnitQuantity n u = evaluatesTo (Numeric n (Just u))

    let evalErrorsWith msg (Left m) = msg == m
        evalErrorsWith _ _ = False

    passPartialFailFnSpec
        "arithmetic expression evaluation"
        ae
        [ ("2", evaluatesToScalar 2)
        , ("3^2", evaluatesToScalar 9)
        , ("2^3^2", evaluatesToScalar 512)
        , ("2+3", evaluatesToScalar 5)
        , ("3-2-1", evaluatesToScalar 0)
        , ("100/2/5", evaluatesToScalar 10)
        , ("3*2", evaluatesToScalar 6)
        , ("2+(5*10)", evaluatesToScalar 52)
        , ("(2+5)*10", evaluatesToScalar 70)
        , ("(2+2)", evaluatesToScalar 4)
        , ("2+3*4^(1/2)", evaluatesToScalar 8)
        , ("2 + 3 * 4 ^ ( 1 / 2 )", evaluatesToScalar 8)
        , ("a", evaluatesToScalar 4)
        , ("foo", evaluatesToUnitQuantity 9 (BaseUnit "kg"))
        , ("foobar", evalErrorsWith "Use of undeclared variable \"foobar\"")
        , ("2 kg", evaluatesToUnitQuantity 2 (BaseUnit "kg"))
        ]
        [ ("2 + 2 || 5 * 7", evaluatesToScalar 4, " || 5 * 7")
        , ("(5*8),(7*4)", evaluatesToScalar 40, ",(7*4)")
        ]
        [ ("+", "Expected a number, variable, or ( expression )", "+")
        , ("(2+2", "Expected ')'", "")
        , ("(2+2(", "Expected ')'", "(")
        , ("2*", "Expected a number, variable, or ( expression )", "")
        ]
