module TestUtils.Assertions.ParserAssertion where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Collector
import X.Control.Parser
import X.Data.ParseError
import X.Utils.LeftToRight

type Input = String
type Remainder = String
type Reason = String

data ParserAssertion a
    = SuccessfulParseAssertion Input a Remainder Title
    | FailedParseAssertion Input Remainder Reason Title

instance ModifyAssertionTitle (ParserAssertion a) where
    modifyAssertionTitle (SuccessfulParseAssertion input value remainder _title) = SuccessfulParseAssertion input value remainder
    modifyAssertionTitle (FailedParseAssertion input remainder reason _title) = FailedParseAssertion input remainder reason

shouldParseTo :: (Show a) => String -> a -> Collector (ParserAssertion a) ()
input `shouldParseTo` val = SuccessfulParseAssertion input val "" (show input <> " should parse to " <> show val) @> singleton

withRemainder :: Collector (ParserAssertion a) b -> String -> Collector (ParserAssertion a) b
withRemainder c newRemainder =
    let modifyAssertion (SuccessfulParseAssertion input val _remainder message) =
            SuccessfulParseAssertion input val newRemainder message
        modifyAssertion failed = failed
     in modifyLast modifyAssertion c

shouldFailWithRemainder :: String -> String -> (String -> Collector (ParserAssertion a) ())
shouldFailWithRemainder input remainder reason = FailedParseAssertion input remainder reason (show input <> " should fail to parse with remainder " <> show remainder) @> singleton

andReason :: (String -> Collector (ParserAssertion a) ()) -> String -> Collector (ParserAssertion a) ()
andReason = ($)

parserAssertion :: (Show a, Eq a) => Parser a -> Collector (ParserAssertion a) b -> Assertion
parserAssertion parser assertions =
    let mapAssertion (SuccessfulParseAssertion input value remainder title) =
            assertEqualVerbose title (Right (remainder, value)) (parse parser input)
        mapAssertion (FailedParseAssertion input remainder reason title) =
            assertEqualVerbose title(Left (ParseError reason remainder)) (parse parser input)
     in getList assertions
            |@>| mapAssertion
            @> sequence_