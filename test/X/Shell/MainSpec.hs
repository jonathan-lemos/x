module X.Shell.MainSpec where

import Control.Monad
import Data.Bifunctor
import Data.Either
import X.Control.Terminal
import X.Data.ParseError
import X.Control.Parser.AST.ArithmeticExpressionSpec
import X.Shell.Main
import Test.Hspec
import Data.List
import Data.List.Split
import X.TestUtils.ArithmeticExpression
import X.TestUtils.State
import X.Data.State.Value
import X.Data.Unit.Unit

ioListToString :: [PrintCmd] -> String
ioListToString = intercalate "" . fmap show

spec :: Spec
spec = parallel $ do
    let sState = mkState [("a", Numeric 4 Nothing), ("foo", Numeric 9 (Just $ BaseUnit "kg"))]

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