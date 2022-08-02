module X.Control.Parser.AST.StatementSpec where
import Test.Hspec
import X.Data.AST.Statement
import X.Control.Parser.AST.Statement
import X.Control.Parser
import Harness.TestCase
import Harness.ParserCase
import X.Data.Value
import X.Data.AST.Assignment
import X.Data.Operator

spec :: Spec
spec = parallel $ do
    describe "assignment tests" $ do
        parserDesc statement "statement" $ do
            "4" `shouldParseTo` StmtValue (Scalar 4)
            "a + 6" `shouldParseTo` StmtValue (AdditiveChain (Variable "a") [(Add, Scalar 6)])

            "a = 4" `shouldParseTo` StmtAssignment (Assignment "a" (Scalar 4))
            "foo = a + 6" `shouldParseTo` StmtAssignment (Assignment "foo" (AdditiveChain (Variable "a") [(Add, Scalar 6)]))

            "_" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` "_"
            "foo =" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
            "2 +" `shouldFailWithReason` "Expected a number, variable, or ( expression )" `andRemainder` ""
