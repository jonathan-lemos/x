{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Shell.MainSpec where

import Data.List
import Data.List.Split
import Test.Framework hiding (reason)
import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion
import TestUtils.Assertions.FunctionAssertion
import X.Control.Terminal
import X.Data.AST.Assignment
import X.Data.AST.Statement (Statement (StmtAssignment, StmtValue))
import X.Data.Context
import X.Data.Operator
import X.Data.ParseError
import X.Data.Value
import X.Shell.Execution
import X.TestUtils.Context
import X.Utils.LeftToRight

ioListToString :: [PrintCmd] -> String
ioListToString = intercalate "" . fmap show

sState :: Context
sState = mkCtx [("a", Scalar 4), ("foo", Scalar 9)]

test_parseStatementWorksOnBasicStatements :: Assertion
test_parseStatementWorksOnBasicStatements = functionAssertion parseStatement $ do
    "f = 2" `shouldEvalTo` Right (StmtAssignment (Assignment "f" (Scalar 2)))
    "2 + 3" `shouldEvalTo` Right (StmtValue (AdditiveChain (Scalar 2) [(Add, Scalar 3)]))

test_parseStatementFailsOnInvalidStatements :: Assertion
test_parseStatementFailsOnInvalidStatements = functionAssertion parseStatement $ do
    "2 +" `shouldEvalTo` Left (ParseError "Expected a number, variable, or ( expression )" "")
    "" `shouldEvalTo` Left (ParseError "Expected a number, variable, or ( expression )" "")

parseAndEval :: String -> Either ParseError (Context, String)
parseAndEval = parseStatement ||@>|| evaluateStatement sState

parseAndEvalResult :: String -> Either ParseError String
parseAndEvalResult = parseAndEval ||@>|| snd

test_evaluateStatementCalculatesBasicExpressions :: Assertion
test_evaluateStatementCalculatesBasicExpressions =
    functionAssertion parseAndEvalResult $ do
        "2 + 3" `shouldEvalTo` Right "5.0"
        "    2     +     3" `shouldEvalTo` Right "5.0"
        "2+3" `shouldEvalTo` Right "5.0"

test_evaluateStatementWorksOnTrailingWhitespace :: Assertion
test_evaluateStatementWorksOnTrailingWhitespace =
    functionAssertion parseAndEvalResult $ do
        "2+3 " `shouldEvalTo` Right "5.0"

test_evaluateStatementReportsSyntaxError :: Assertion
test_evaluateStatementReportsSyntaxError =
    functionAssertion parseAndEvalResult $ do
        "2+" `shouldEvalTo` Left (ParseError{reason = "Expected a number, variable, or ( expression )", currentInput = ""})

execAndGetLines :: Context -> Int -> String -> [String]
execAndGetLines a b c =
    execute a b c
        @> snd
        @> fmap show
        @> intercalate ""
        @> splitOn "\n"
        @> init

test_executePrintsValue :: Assertion
test_executePrintsValue =
    basicAssertion $ do
        execAndGetLines sState 80 "123.45" `shouldBe` ["123.45"]

test_executePrintsError :: Assertion
test_executePrintsError =
    basicAssertion $ do
        execAndGetLines sState 80 "2 + _ + 7"
            `shouldBe` [ "error: Expected a number, variable, or ( expression )"
                       , ""
                       , "2 + _ + 7"
                       , "    ^"
                       ]
