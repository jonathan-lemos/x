{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Control.Parser.AST.AssignmentSpec where

import Test.Framework
import X.Control.Parser.AST.Assignment
import X.Data.AST.Assignment
import X.Data.Value
import Test.Framework.TestInterface
import TestUtils.Assertions.ParserAssertion
import TestUtils.DSL.Value

test_assignment :: Assertion
test_assignment =
    parserAssertion assignment $ do
        "f = 2" `shouldParseTo` Assignment "f" (Scalar 2)
        "f = a + 2" `shouldParseTo` Assignment "f" ("a" @+ Scalar 2)

        "f = 2;foo" `shouldParseTo` Assignment "f" (Scalar 2) `withRemainder` ";foo"
        "f = a + 2 ;foo" `shouldParseTo` Assignment "f" (Variable "a" @+ Scalar 2) `withRemainder` " ;foo"

        "_" `shouldFailWithRemainder` "_" `andReason` "Expected an A-z character"
        "foo = " `shouldFailWithRemainder` "" `andReason` "Expected a number, variable, or ( expression )"
