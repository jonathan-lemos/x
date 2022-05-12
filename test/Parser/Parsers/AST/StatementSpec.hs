module Parser.Parsers.AST.StatementSpec where
import Test.Hspec
import Types.AST.Statement
import Parser.Parsers.AST.Statement (statement)
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpressionTestUtils (hasError)

spec :: Spec
spec = do
    describe "assignment tests" $ do
        let isAssignment (Right ("", StmtAssignment _)) = True
            isAssignment _ = False

        let isExpr (Right ("", StmtExpr _)) = True
            isExpr _ = False

        let stmt = parse statement

        it "parses assignments as assignments" $ do
            stmt "a = 4" `shouldSatisfy` isAssignment
            stmt "foo = a + 6" `shouldSatisfy` isAssignment

        it "parses expressions as expressions" $ do
            stmt "4" `shouldSatisfy` isExpr
            stmt "a + 6" `shouldSatisfy` isExpr

        it "delivers correct error messages" $ do
            stmt "_" `shouldSatisfy` hasError "Expected a number, variable, or ( expression )" "_"
            stmt "foo =" `shouldSatisfy` hasError "Expected a number, variable, or ( expression )" ""
            stmt "2 +" `shouldSatisfy` hasError "Expected a number, variable, or ( expression )" ""
