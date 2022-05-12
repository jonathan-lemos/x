module Parser.Parsers.AST.AssignmentSpec where

import Test.Hspec

import Parser.Parsers.AST.Assignment
import Parser.Parser
import Types.AST.Assignment
import Parser.Parsers.AST.ArithmeticExpressionTestUtils
import Types.Evaluatable.Evaluatable

spec :: Spec
spec = do
    describe "assignment tests" $ do
        let a = parse assignment

        let isValidAssignment s (Right ("", Assignment x _)) = s == x
            isValidAssignment _ _ = False

        let state = mkState [("a", 4), ("b", 9)]

        let hasValue v (Right ("", Assignment _ parsed)) = evaluate parsed state == Right v
            hasValue _ _ = False

        it "parses a basic assignment" $ do
            a "f = 2" `shouldSatisfy` isValidAssignment "f"

        it "parses a assignment with an expression" $ do
            a "f = a + 2" `shouldSatisfy` isValidAssignment "f"

        it "evaluates the correct value for basic assignment" $ do
            a "f = 2" `shouldSatisfy` hasValue 2

        it "evaluates the correct value for expression assignment" $ do
            a "f = a + 2" `shouldSatisfy` hasValue 6

        it "delivers the correct error message for partial parse" $ do
            a "_" `shouldSatisfy` hasError "Expected an identifier, which is a sequence of lowercase characters ('_' is not)" "_"
            a "foo = " `shouldSatisfy` hasError "Expected a number, variable, or ( expression )" ""
