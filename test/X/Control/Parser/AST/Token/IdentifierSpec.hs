{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Control.Parser.AST.Token.IdentifierSpec where

import Test.Framework
import X.Control.Parser.AST.Token.Identifier
import Control.Applicative
import X.Data.ParseError
import Test.Framework.TestInterface
import TestUtils.Assertions.ParserAssertion

test_identifier :: Assertion
test_identifier =
    parserAssertion identifier $ do
        "abc" `shouldParseTo` "abc"
        "d" `shouldParseTo` "d"

        "abc def" `shouldParseTo` "abc" `withRemainder` " def"

        "_" `shouldFailWithRemainder` "_" `andReason` "Expected an A-z character"
        "_abc" `shouldFailWithRemainder` "_abc" `andReason` "Expected an A-z character"
        " abc" `shouldFailWithRemainder` " abc" `andReason` "Expected an A-z character"
