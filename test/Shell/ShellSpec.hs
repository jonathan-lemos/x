module Shell.ShellSpec where

import Control.Monad
import Data.Bifunctor
import Data.Either
import IO.IOCmd
import Parser.Error
import Parser.Parsers.AST.ArithmeticExpressionSpec
import Shell.Shell
import Test.Hspec
import Data.List
import Data.List.Split
import Parser.Parsers.AST.ArithmeticExpressionTestUtils

ioListToString :: [IOCmd] -> String
ioListToString = intercalate "" . fmap show

spec :: Spec
spec = do
    let sState = mkState [("a", 4), ("foo", 9)]

    describe "parseCommand tests" $ do
        it "parses basic expressions" $ do
            parseCommand "2 + 2" `shouldSatisfy` isRight
            parseCommand "2 + 2 ^ 7" `shouldSatisfy` isRight

        it "errors on invalid expressions" $ do
            parseCommand "2 +" `shouldSatisfy` isLeft
            parseCommand "" `shouldSatisfy` isLeft

    describe "executeStatement tests" $ do
        let es = first show . parseCommand >=> executeStatement sState
        let esRes = second snd . es
        let esState = second fst . es

        it "calculates basic expressions properly" $ do
            esRes "2 + 3" `shouldBe` Right "5.0"
            esRes "    2     +     3" `shouldBe` Right "5.0"
            esRes "2+3" `shouldBe` Right "5.0"

        it "executes expression with trailing whitespace" $ do
            esRes "2+3 " `shouldBe` Right "5.0"

        it "reports syntax error on invalid expressions" $ do
            esRes "2+" `shouldBe` Left "ParseError {reason = \"Expected a number, variable, or ( expression )\", currentInput = \"\"}"

    describe "execute tests" $ do
        let execLines a b c = init . splitOn "\n" . intercalate "" . snd $ second (fmap show) (execute a b c)

        it "prints value if value is present" $ do
            execLines sState 80 "123.45" `shouldBe` ["123.45"]

        it "prints error if error is present" $ do
            execLines sState 80 "2 + _ + 7"
                `shouldBe` [ "error: Expected a number, variable, or ( expression )"
                           , ""
                           , "2 + _ + 7"
                           , "    ^"
                           ]
