module X.Shell.MainSpec where

import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.List
import Data.List.Split
import Harness.TestCase
import Test.Hspec
import X.Control.Parser.AST.ArithmeticExpressionSpec
import X.Control.Terminal
import X.Control.Try
import X.Data.ParseError
import X.Data.State.Value
import X.Data.Unit.Unit
import X.Shell.Main
import X.TestUtils.ArithmeticExpression
import X.TestUtils.State
import X.Utils.Function
import X.Utils.Functor
import X.Utils.Try (eitherToTry)

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

    describe "evaluateStatement tests" $ do
        let parseAndEval = parseCommand |> first show |> eitherToTry >=> evaluateStatement sState
        let parseAndEvalResult = parseAndEval |> fmap snd
        let parseAndEvalState = parseAndEval |> fmap fst

        desc "calculates basic expressions properly" $ do
            parseAndEvalResult "2 + 3" `shouldEq` Success "5.0"
            parseAndEvalResult "    2     +     3" `shouldEq` Success "5.0"
            parseAndEvalResult "2+3" `shouldEq` Success "5.0"

        desc "executes expression with trailing whitespace" $ do
            parseAndEvalResult "2+3 " `shouldEq` Success "5.0"

        desc "reports syntax error on invalid expressions" $ do
            parseAndEvalResult "2+" `shouldEq` Failure "ParseError {reason = \"Expected a number, variable, or ( expression )\", currentInput = \"\"}"

    describe "execute tests" $ do
        let exec a b c =
                execute a b c
                    >$ snd
                    >$ fmap show
                    >$ intercalate ""
                    >$ splitOn "\n"
                    >$ init

        desc "prints value if value is present" $ do
            exec sState 80 "123.45" `shouldEq` ["123.45"]

        desc "prints error if error is present" $ do
            exec sState 80 "2 + _ + 7"
                `shouldEq` [ "error: Expected a number, variable, or ( expression )"
                           , ""
                           , "2 + _ + 7"
                           , "    ^"
                           ]
