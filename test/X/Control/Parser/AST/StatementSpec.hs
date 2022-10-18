{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Control.Parser.AST.StatementSpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.ParserAssertion
import X.Control.Parser.AST.Statement
import X.Data.AST.Assignment
import X.Data.AST.Statement
import X.Data.Operator
import X.Data.Value
import X.TestUtils.Value

test_statement :: Assertion
test_statement =
    parserAssertion statement $ do
        "4" `shouldParseTo` StmtValue (Scalar 4)
        "a + 6" `shouldParseTo` StmtValue (ac (Variable "a") [(Add, Scalar 6)])

        "a = 4" `shouldParseTo` StmtAssignment (Assignment "a" (Scalar 4))
        "foo = a + 6" `shouldParseTo` StmtAssignment (Assignment "foo" (ac (Variable "a") [(Add, Scalar 6)]))

        "_" `shouldFailWithRemainder` "_" `andReason` "Expected a number, variable, or ( expression )"
        "foo =" `shouldFailWithRemainder` "" `andReason` "Expected a number, variable, or ( expression )"
        "2 +" `shouldFailWithRemainder` "" `andReason` "Expected a number, variable, or ( expression )"
