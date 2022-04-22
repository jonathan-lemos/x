module ShellSpec where
import Test.Hspec
import Shell (execute)

spec :: Spec
spec = do
    describe "execute tests" $ do
        it "executes basic expressions properly" $ do
            execute "2 + 3" `shouldBe` Right "5.0"
            execute "    2     +     3" `shouldBe` Right "5.0"
            execute "2+3" `shouldBe` Right "5.0"

        it "executes expression with trailing whitespace" $ do
            execute "2+3 " `shouldBe` Right "5.0"

        it "reports syntax error on invalid expressions" $ do
            execute "2+" `shouldBe` Left "Syntax Error"
