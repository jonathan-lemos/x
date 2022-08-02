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
import X.Shell.Main
import X.Utils.Try (eitherToTry)
import X.TestUtils.Context
import X.Data.Value
import X.Shell.Execution
import X.Data.AST.Statement (Statement(StmtValue, StmtAssignment))
import X.Data.Operator
import X.Data.AST.Assignment
import X.Utils.LeftToRight

ioListToString :: [PrintCmd] -> String
ioListToString = intercalate "" . fmap show

spec :: Spec
spec = parallel $ do
    let sState = mkCtx [("a", Scalar 4), ("foo", Scalar 9)]

    describe "parseCommand tests" $ do
        desc "parses basic expressions" $ do
            parseStatement "f = 2" `shouldEq` Right (StmtAssignment (Assignment "f" (Scalar 2)))
            parseStatement "2 + 3" `shouldEq` Right (StmtValue (AdditiveChain (Scalar 2) [(Add, Scalar 3)]))

        desc "errors on invalid expressions" $ do
            parseStatement "2 +" `shouldEq` Left (ParseError "Expected a number, variable, or ( expression )" "")
            parseStatement "" `shouldEq` Left (ParseError "Expected a number, variable, or ( expression )" "")

    describe "evaluateStatement tests" $ do
        let parseAndEval = parseStatement ||@>|| evaluateStatement sState
        let parseAndEvalResult = parseAndEval ||@>|| snd
        let parseAndEvalState = parseAndEval ||@>|| fst

        desc "calculates basic expressions properly" $ do
            parseAndEvalResult "2 + 3" `shouldEq` Right "5.0"
            parseAndEvalResult "    2     +     3" `shouldEq` Right "5.0"
            parseAndEvalResult "2+3" `shouldEq` Right "5.0"

        desc "executes expression with trailing whitespace" $ do
            parseAndEvalResult "2+3 " `shouldEq` Right "5.0"

        desc "reports syntax error on invalid expressions" $ do
            parseAndEvalResult "2+" `shouldEq` Left (ParseError {reason = "Expected a number, variable, or ( expression )", currentInput = ""})

    describe "execute tests" $ do
        let exec a b c =
                execute a b c
                    @> snd
                    @> fmap show
                    @> intercalate ""
                    @> splitOn "\n"
                    @> init

        desc "prints value if value is present" $ do
            exec sState 80 "123.45" `shouldEq` ["123.45"]

        desc "prints error if error is present" $ do
            exec sState 80 "2 + _ + 7"
                `shouldEq` [ "error: Expected a number, variable, or ( expression )"
                           , ""
                           , "2 + _ + 7"
                           , "    ^"
                           ]
