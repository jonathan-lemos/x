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
import Harness.With


isParentheses :: Factor -> Bool
isParentheses (Parentheses _) = True
isParentheses _ = False

spec :: Spec
spec = parallel $ do

    parserDesc factor "factor" $ do
        "2" `shouldParseTo` FactorScalar (Number 2)
        "(2)" `shouldParseAndSatisfy` isParentheses
        "( 2 )" `shouldParseAndSatisfy` isParentheses
        "(2 + 2)" `shouldParseAndSatisfy` isParentheses
        "a" `shouldParseTo` FactorScalar (Variable "a")
        "foobar" `shouldParseTo` FactorScalar (Variable "foobar")

        "2 || 5" `shouldParseTo` FactorScalar (Number 2) `withRemainder` " || 5"
        "(2) || 5" `shouldParseAndSatisfy` isParentheses `withRemainder` " || 5"

        "@" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` "@"

    let state = mkState [("a", Numeric 4 Nothing), ("foo", Numeric 9 (Just $ BaseUnit "kg"))]
    let gu s = fromJust $ getUnit s state

    let ae = (`toValue` state) <$> arithmeticExpression

    let evaluatesTo n (Success r) = n == r
        evaluatesTo _ _ = False

    let evaluatesToScalar n = evaluatesTo (Numeric n Nothing)

    let evaluatesToUnitQuantity n u = evaluatesTo (Numeric n (Just u))

    let evalErrorsWith msg (Failure m) = msg == m
        evalErrorsWith _ _ = False

    parserDesc ae "arithmetic expression evaluation" $ do
        "2" `shouldParseAndSatisfy` evaluatesToScalar 2
        "3^2" `shouldParseAndSatisfy` evaluatesToScalar 9
        "2^3^2" `shouldParseAndSatisfy` evaluatesToScalar 512
        "2+3" `shouldParseAndSatisfy` evaluatesToScalar 5
        "3-2-1" `shouldParseAndSatisfy` evaluatesToScalar 0
        "100/2/5" `shouldParseAndSatisfy` evaluatesToScalar 10
        "3*2" `shouldParseAndSatisfy` evaluatesToScalar 6
        "2+(5*10)" `shouldParseAndSatisfy` evaluatesToScalar 52
        "(2+5)*10" `shouldParseAndSatisfy` evaluatesToScalar 70
        "(2+2)" `shouldParseAndSatisfy` evaluatesToScalar 4
        "2+3*4^(1/2)" `shouldParseAndSatisfy` evaluatesToScalar 8
        "2 + 3 * 4 ^ ( 1 / 2 )" `shouldParseAndSatisfy` evaluatesToScalar 8
        "a" `shouldParseAndSatisfy` evaluatesToScalar 4
        "foo" `shouldParseAndSatisfy` evaluatesToUnitQuantity 9 (gu "kg")
        "foobar" `shouldParseAndSatisfy` evalErrorsWith "Use of undeclared variable \"foobar\""
        "2 kg" `shouldParseAndSatisfy` evaluatesToUnitQuantity 2 (gu "kg")
        "2 kg*m" `shouldParseAndSatisfy` evaluatesToUnitQuantity 2 (gu "kg" `unitMult` gu "m")
        -- "2kg*3m" `shouldParseAndSatisfy` evaluatesToUnitQuantity 6 (gu "kg" `unitMult` gu "m")
        "2kg * 3m" `shouldParseAndSatisfy` evaluatesToUnitQuantity 6 (gu "kg" `unitMult` gu "m")
        "2 kg * 3 m" `shouldParseAndSatisfy` evaluatesToUnitQuantity 6 (gu "kg" `unitMult` gu "m")

        "2 + 2 || 5 * 7" `shouldParseAndSatisfy` evaluatesToScalar 4 `withRemainder` " || 5 * 7"
        "(5*8),(7*4)" `shouldParseAndSatisfy` evaluatesToScalar 40 `withRemainder` ",(7*4)"

        "+" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` "+"
        "(2+2" `shouldFailWithReason` "Expected ')'" `andRemainder` ""
        "(2+2(" `shouldFailWithReason` "Expected ')'" `andRemainder` "("
        "2*" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
