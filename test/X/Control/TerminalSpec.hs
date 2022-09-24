{-# OPTIONS_GHC -F -pgmF htfpp #-}
module X.Control.TerminalSpec where

import Test.Framework
import X.Control.Terminal
import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion

test_terminalLineAppendsNewline :: Assertion
test_terminalLineAppendsNewline = basicAssertion $ do
    line "foo" `shouldBe` PrintCmd [] "foo\n"

test_terminalNewlinePrintsNewline :: Assertion
test_terminalNewlinePrintsNewline = basicAssertion $ do
    newline `shouldBe` PrintCmd [] "\n"
