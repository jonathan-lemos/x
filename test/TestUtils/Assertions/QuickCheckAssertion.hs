module TestUtils.Assertions.QuickCheckAssertion where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Collector
import X.Utils.LeftToRight

data QuickCheckAssertion a = QuickCheckAssertion (a -> Bool) String

instance ModifyAssertionTitle (QuickCheckAssertion a) where
    modifyAssertionTitle (QuickCheckAssertion f _title) = QuickCheckAssertion f

satisfies :: (a -> Bool) -> Collector (QuickCheckAssertion a) ()
satisfies pred = QuickCheckAssertion pred "" @> singleton

quickCheckAssertion :: (Show a, Arbitrary a, Show b, Eq b) => (a -> b) -> Collector (QuickCheckAssertion b) c -> Assertion
quickCheckAssertion f as =
    getList as
        |@>| ( \(QuickCheckAssertion pred title) ->
                (\x -> f x @> pred)
                    @> label title
                    @> withMaxSuccess 1000
                    @> quickCheck
             )
            @> sequence_
