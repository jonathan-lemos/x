module X.Control.Parser.AST.StatementSpec where
import Test.Hspec
import X.Data.AST.Statement
import X.Control.Parser.AST.Statement (statement)
import X.Control.Parser
import X.TestUtils.ArithmeticExpression
import X.TestUtils.Parser (shouldFailWithMsgAndCi)

spec :: Spec
spec = parallel $ do
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
            (statement, "_") `shouldFailWithMsgAndCi` ("Expected a number, variable, or ( expression )", "_")
            (statement, "foo =") `shouldFailWithMsgAndCi` ("Expected a number, variable, or ( expression )", "")
            (statement, "2 +") `shouldFailWithMsgAndCi` ("Expected a number, variable, or ( expression )", "")