{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Control.Parser.AST.ArithmeticExpressionSpec where

import Test.Framework
import Test.Framework.TestInterface
import X.Control.Parser.AST.ArithmeticExpression
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Evaluate
import X.TestUtils.Context
import X.Utils.LeftToRight
import TestUtils.Assertions.ParserAssertion

test_factor :: Assertion
test_factor = do
    parserAssertion factor $ do
        "2" `shouldParseTo` Scalar 2
        "(2)" `shouldParseTo` Scalar 2
        "( 2 )" `shouldParseTo` Scalar 2
        "(2 + 2)" `shouldParseTo` AdditiveChain (Scalar 2) [(Add, Scalar 2)]
        "a" `shouldParseTo` Variable "a"
        "foobar" `shouldParseTo` Variable "foobar"

        "2 || 5" `shouldParseTo` Scalar 2 `withRemainder` " || 5"
        "(2) || 5" `shouldParseTo` Scalar 2 `withRemainder` " || 5"

        "@" `shouldFailWithRemainder` "@" `andReason` "Expected a number, variable, or ( expression )"

test_evaluation :: Assertion
test_evaluation = do
    let state = mkCtx [("a", Scalar 4), ("foo", Scalar 9)]

    let evaluateAdditiveExpression = additiveExpression |@>| (`evaluateValue` state)

    parserAssertion evaluateAdditiveExpression $ do
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

        "+" `shouldFailWithRemainder` "+" `andReason` "Expected a number, variable, or ( expression )"
        "(2+2" `shouldFailWithRemainder` "" `andReason` "Expected ')'"
        "(2+2(" `shouldFailWithRemainder` "(" `andReason` "Expected ')'"
        "2*" `shouldFailWithRemainder` "" `andReason` "Expected a number, variable, or ( expression )"
