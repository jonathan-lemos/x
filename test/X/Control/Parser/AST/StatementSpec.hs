module X.Control.Parser.AST.StatementSpec where
import Test.Hspec
import X.Data.AST.Statement
import X.Control.Parser.AST.Statement (statement)
import X.Control.Parser
import X.TestUtils.ArithmeticExpression
import Harness.TestCase
import Harness.ParserCase

spec :: Spec
spec = parallel $ do
    describe "assignment tests" $ do
        let isAssignment (Right ("", StmtAssignment _)) = True
            isAssignment _ = False

        let isExpr (Right ("", StmtExpr _)) = True
            isExpr _ = False

        let stmt = parse statement

        desc "parses assignments as assignments" $ do
            stmt "a = 4" `should` isAssignment
            stmt "foo = a + 6" `should` isAssignment

        desc "parses expressions as expressions" $ do
            stmt "4" `should` isExpr
            stmt "a + 6" `should` isExpr

        parserDesc statement "delivers correct error messages" $ do
            "_" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` "_"
            "foo =" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
            "2 +" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
