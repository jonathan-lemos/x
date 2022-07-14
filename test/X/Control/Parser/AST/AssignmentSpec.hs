module X.Control.Parser.AST.AssignmentSpec where

import Test.Hspec

import X.Evaluation.ToValue
import X.Control.Parser
import X.Control.Parser.AST.Assignment
import X.Data.State.Value
import X.TestUtils.ArithmeticExpression
import X.TestUtils.State
import X.Data.AST.Assignment
import X.Data.Unit.Unit
import X.Control.Try
import Harness.ParserCase
import Harness.With

spec :: Spec
spec = parallel $ do
    let isValidAssignment s (Assignment x _) = s == x

    let state = mkState [("a", Numeric 4 Nothing), ("b", Numeric 9 (Just $ BaseUnit "kg"))]

    let hasValue v (Assignment _ parsed) = toValue parsed state == Success v

    let hasScalar = hasValue . (`Numeric` Nothing)

    let hasUnitQuantity u q = hasValue (Numeric u (Just q))

    parserDesc assignment "assignment" $ do
        "f = 2" `shouldParseAndSatisfy` isValidAssignment "f"
        "f = a + 2" `shouldParseAndSatisfy` isValidAssignment "f"
        "f = 2" `shouldParseAndSatisfy` hasScalar 2
        "f = a + 2" `shouldParseAndSatisfy` hasScalar 6

        "f = 2;foo" `shouldParseAndSatisfy` isValidAssignment "f" `withRemainder` ";foo"
        "f = a + 2 ;foo" `shouldParseAndSatisfy` isValidAssignment "f" `withRemainder` " ;foo"
        "f = 2 ;foo" `shouldParseAndSatisfy` hasScalar 2 `withRemainder` " ;foo"
        "f = a + 2 ;foo" `shouldParseAndSatisfy` hasScalar 6 `withRemainder` " ;foo"

        "_" `shouldFailWithReason` "Expected an A-z character" `andRemainder` "_"
        "foo = " `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
