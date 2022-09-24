module TestUtils.Assertions.BasicAssertion where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Collector
import X.Utils.LeftToRight

data BasicAssertion = BasicAssertion Bool String

shouldBe :: (Show a, Eq a) => a -> a -> Collector BasicAssertion ()
shouldBe a b = BasicAssertion (a == b) (show a <> " should be " <> show b) @> singleton

assert :: Bool -> Collector BasicAssertion ()
assert a = BasicAssertion a "assert" @> singleton

instance ModifyAssertionTitle BasicAssertion where
    modifyAssertionTitle (BasicAssertion value _oldMsg) = BasicAssertion value

basicAssertion :: Collector BasicAssertion a -> Assertion
basicAssertion =
    getList
        ||@>|| (\(BasicAssertion value message) -> assertBoolVerbose message value)
        |@>| sequence_
