module Shell.ShellSpec where
import Test.Hspec
import Shell.Shell
import Parser.Error
import IO.IOCmd
import Data.List

ioListToString :: [IOCmd] -> String
ioListToString = intercalate "" . fmap show

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
            calculate "2+" `shouldBe` Left (ParseError "Expected a number or ( expression )" "")

    describe "printResult tests" $ do
        it "prints value if value is present" $ do
            show <$> printResult 80 "123.45" (Right "123.45") `shouldBe` ["123.45\n"]

        it "prints error if error is present" $ do
            ioListToString (printResult 80 "2 + 2" $ Left (ParseError "it failed" "+ 2"))
            `shouldBe`
            unlines [
                "error: it failed",
                "",
                "2 + 2",
                "  ^"
            ]
