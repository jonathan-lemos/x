module Parser.Parsers.AST.AssignmentSpec where

import Test.Hspec

import Evaluation.ToValue
import Parser.Parser
import Parser.Parsers.AST.Assignment
import State.Value
import TestUtils.ArithmeticExpression
import TestUtils.Parser
import TestUtils.State
import Types.AST.Assignment
import Unit.Unit

spec :: Spec
spec = parallel $ do
    let isValidAssignment s (Assignment x _) = s == x

    let state = mkState [("a", Numeric 4 Nothing), ("b", Numeric 9 (Just $ BaseUnit "kg"))]

    let hasValue v (Assignment _ parsed) = toValue parsed state == Right v

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
