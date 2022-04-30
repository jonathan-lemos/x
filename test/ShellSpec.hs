module ShellSpec where
import Test.Hspec
import Shell
import Shell (calculate)
import Parser.Error

spec :: Spec
spec = do
    describe "calculate tests" $ do
        it "calculates basic expressions properly" $ do
            calculate "2 + 3" `shouldBe` Right "5.0"
            calculate "    2     +     3" `shouldBe` Right "5.0"
            calculate "2+3" `shouldBe` Right "5.0"

        it "executes expression with trailing whitespace" $ do
            calculate "2+3 " `shouldBe` Right "5.0"

        it "reports syntax error on invalid expressions" $ do
            calculate "2+" `shouldBe` Left (ParseError "Unexpected end of parse" "+")
