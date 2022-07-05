module X.Control.Parser.AST.ArithmeticExpressionSpec where

import Data.Bifunctor
import Data.Number.CReal
import X.Evaluation.ToValue
import X.Data.ParseError
import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import X.Data.State.XState
import Test.Hspec
import X.TestUtils.ArithmeticExpression
import X.TestUtils.Parser
import X.TestUtils.State
import X.Data.AST.ArithmeticExpression
import X.Data.AST.Token.Scalar
import X.Data.AST.UnitExpression
import X.Data.State.Value
import X.Data.Unit.Unit
import X.Data.Unit.Arithmetic
import Data.Maybe
import X.Control.Try
import Harness.TestCase
import Harness.ParserCase


beParentheses :: Factor -> Bool
beParentheses (Parentheses _) = True
beParentheses _ = False

spec :: Spec
spec = parallel $ do

    parserDesc factor "factor" $ do
        "2" `shouldTotallyParseTo` FactorScalar (Number 2)
        "(2)" `shouldTotallyParseAndSatisfy` beParentheses
        "( 2 )" `shouldTotallyParseAndSatisfy` beParentheses
        "(2 + 2)" `shouldTotallyParseAndSatisfy` beParentheses
        "a" `shouldTotallyParseAndSatisfy` FactorScalar (Variable "a")
        "foobar" `shouldBe` FactorScalar (Variable "foobar")

        "2 || 5" `shouldBe`

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
    let gu s = fromJust $ getUnit s state

    let ae = (`toValue` state) <$> arithmeticExpression

    let evaluatesTo n (Success r) = n == r
        evaluatesTo _ _ = False

    let evaluatesToScalar n = evaluatesTo (Numeric n Nothing)

    let evaluatesToUnitQuantity n u = evaluatesTo (Numeric n (Just u))

    let evalErrorsWith msg (Failures m) = msg == m
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
        , ("foo", evaluatesToUnitQuantity 9 (gu "kg"))
        , ("foobar", evalErrorsWith ["Use of undeclared variable \"foobar\""])
        , ("2 kg", evaluatesToUnitQuantity 2 (gu "kg"))
        , ("2 kg*m", evaluatesToUnitQuantity 2 (gu "kg" `unitMult` gu "m"))
        -- , ("2kg*3m", evaluatesToUnitQuantity 6 (gu "kg" `unitMult` gu "m"))
        , ("2kg * 3m", evaluatesToUnitQuantity 6 (gu "kg" `unitMult` gu "m"))
        , ("2 kg * 3 m", evaluatesToUnitQuantity 6 (gu "kg" `unitMult` gu "m"))
        ]
        [ ("2 + 2 || 5 * 7", evaluatesToScalar 4, " || 5 * 7")
        , ("(5*8),(7*4)", evaluatesToScalar 40, ",(7*4)")
        ]
        [ ("+", "Expected a number, variable, or ( expression )", "+")
        , ("(2+2", "Expected ')'", "")
        , ("(2+2(", "Expected ')'", "(")
        , ("2*", "Expected a number, variable, or ( expression )", "")
        ]
