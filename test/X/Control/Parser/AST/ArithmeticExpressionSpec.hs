module X.Control.Parser.AST.ArithmeticExpressionSpec where

import Data.Bifunctor
import Data.Number.CReal
import X.Data.ParseError
import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import Test.Hspec
import Data.Maybe
import X.Control.Try
import Harness.TestCase
import Harness.ParserCase
import Harness.With
import X.Data.Value
import X.Data.Operator
import X.TestUtils.Context
import X.Data.Value.Evaluate


spec :: Spec
spec = parallel $ do

    parserDesc factor "factor" $ do
        "2" `shouldParseTo` Scalar 2
        "(2)" `shouldParseTo` Scalar 2
        "( 2 )" `shouldParseTo` Scalar 2
        "(2 + 2)" `shouldParseTo` AdditiveChain (Scalar 2) [(Add, Scalar 2)]
        "a" `shouldParseTo` Variable "a"
        "foobar" `shouldParseTo` Variable "foobar"

        "2 || 5" `shouldParseTo` Scalar 2 `withRemainder` " || 5"
        "(2) || 5" `shouldParseTo` Scalar 2 `withRemainder` " || 5"

        "@" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` "@"

    let state = mkCtx [("a", Scalar 4), ("foo", Scalar 9)]

    let evaluateAdditiveExpression = (`evaluateValue` state) <$> additiveExpression

    parserDesc evaluateAdditiveExpression "arithmetic expression evaluation" $ do
        "2" `shouldParseTo` Scalar 2
        "3^2" `shouldParseTo` Scalar 9
        "2^3^2" `shouldParseTo` Scalar 512
        "2+3" `shouldParseTo` Scalar 5
        "3-2-1" `shouldParseTo` Scalar 0
        "100/2/5" `shouldParseTo` Scalar 10
        "3*2" `shouldParseTo` Scalar 6
        "2+(5*10)" `shouldParseTo` Scalar 52
        "(2+5)*10" `shouldParseTo` Scalar 70
        "(2+2)" `shouldParseTo` Scalar 4
        "2+3*4^(1/2)" `shouldParseTo` Scalar 8
        "2 + 3 * 4 ^ ( 1 / 2 )" `shouldParseTo` Scalar 8
        "a" `shouldParseTo` Scalar 4
        "foo" `shouldParseTo` Scalar 9
        "foobar" `shouldParseTo` Variable "foobar"

        "2 + 2 || 5 * 7" `shouldParseTo` Scalar 4 `withRemainder` " || 5 * 7"
        "(5*8),(7*4)" `shouldParseTo` Scalar 40 `withRemainder` ",(7*4)"

        "+" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` "+"
        "(2+2" `shouldFailWithReason` "Expected ')'" `andRemainder` ""
        "(2+2(" `shouldFailWithReason` "Expected ')'" `andRemainder` "("
        "2*" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
