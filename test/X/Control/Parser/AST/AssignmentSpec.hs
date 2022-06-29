module X.Control.Parser.AST.AssignmentSpec where

import Test.Hspec

import X.Evaluation.ToValue
import X.Control.Parser
import X.Control.Parser.AST.Assignment
import X.Data.State.Value
import X.TestUtils.ArithmeticExpression
import X.TestUtils.Parser
import X.TestUtils.State
import X.Data.AST.Assignment
import X.Data.Unit.Unit
import X.Control.Try

spec :: Spec
spec = parallel $ do
    let isValidAssignment s (Assignment x _) = s == x

    let state = mkState [("a", Numeric 4 Nothing), ("b", Numeric 9 (Just $ BaseUnit "kg"))]

    let hasValue v (Assignment _ parsed) = toValue parsed state == Success v

    let hasScalar = hasValue . (`Numeric` Nothing)

    let hasUnitQuantity u q = hasValue (Numeric u (Just q))

    passPartialFailFnSpec
        "assignment"
        assignment
        [ ("f = 2", isValidAssignment "f")
        , ("f = a + 2", isValidAssignment "f")
        , ("f = 2", hasScalar 2)
        , ("f = a + 2", hasScalar 6)
        ]
        [("f = 2;foo", isValidAssignment "f", ";foo")
        , ("f = a + 2 ;foo", isValidAssignment "f", " ;foo")
        , ("f = 2 ;foo", hasScalar 2, " ;foo")
        , ("f = a + 2 ;foo", hasScalar 6, " ;foo")]
        [ ("_", "Expected an A-z character", "_")
        , ("foo = ", "Expected a number, variable, or ( expression )", "")
        ]
