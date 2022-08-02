module X.Control.Parser.AST.AssignmentSpec where

import Test.Hspec

import X.Control.Parser
import X.Control.Parser.AST.Assignment
import X.Data.AST.Assignment
import X.Control.Try
import Harness.ParserCase
import Harness.With
import X.TestUtils.Context
import X.Data.Value
import X.Data.Operator

spec :: Spec
spec = parallel $ do
    parserDesc assignment "assignment" $ do
        "f = 2" `shouldParseTo` Assignment "f" (Scalar 2)
        "f = a + 2" `shouldParseTo` Assignment "f" (AdditiveChain (Variable "a") [(Add, Scalar 2)])

        "f = 2;foo" `shouldParseTo` Assignment "f" (Scalar 2) `withRemainder` ";foo"
        "f = a + 2 ;foo" `shouldParseTo` Assignment "f" (AdditiveChain (Variable "a") [(Add, Scalar 2)]) `withRemainder` " ;foo"

        "_" `shouldFailWithReason` "Expected an A-z character" `andRemainder` "_"
        "foo = " `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
