{-# OPTIONS_GHC -F -pgmF htfpp #-}
module X.Control.Parser.Combinator.Branch.CheckSpec where

import Test.Framework
import X.Control.Parser.Combinator.Branch.Check
import X.Control.Parser.AST.Token.Identifier
import X.Utils.LeftToRight
import Test.Framework.TestInterface
import TestUtils.Assertions.ParserAssertion

test_checkIdentifierLengthLt5 :: Assertion
test_checkIdentifierLengthLt5 = do
    let sampleParser = check (length |@>| (< 5)) (<> " failed") identifier

    parserAssertion sampleParser $ do
        "abc" `shouldParseTo` "abc"

        "abc def" `shouldParseTo` "abc" `withRemainder` " def"

        "_" `shouldFailWithRemainder` "_" `andReason` "Expected an A-z character"
        "abcde" `shouldFailWithRemainder` "" `andReason` "abcde failed"
        "abcde ghi" `shouldFailWithRemainder` " ghi" `andReason` "abcde failed"
