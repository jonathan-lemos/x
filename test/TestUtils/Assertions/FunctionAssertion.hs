module TestUtils.Assertions.FunctionAssertion where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Collector
import X.Utils.LeftToRight

data FunctionAssertion a b = EqAssertion a b String | PredicateAssertion a (b -> Bool) String

instance ModifyAssertionTitle (FunctionAssertion a b) where
    modifyAssertionTitle (EqAssertion a b _title) = EqAssertion a b
    modifyAssertionTitle (PredicateAssertion a f _title) = PredicateAssertion a f

shouldEvalAndSatisfy :: a -> (b -> Bool) -> Collector (FunctionAssertion a b) ()
shouldEvalAndSatisfy a f = PredicateAssertion a f "" @> singleton

shouldEvalTo :: a -> b -> Collector (FunctionAssertion a b) ()
shouldEvalTo a b = EqAssertion a b "" @> singleton

functionAssertion :: (Show a, Show b, Eq b) => (a -> b) -> Collector (FunctionAssertion a b) c -> Assertion
functionAssertion f as =
    let mapAssertion (EqAssertion a b title) = assertEqualVerbose title (f a) b
        mapAssertion (PredicateAssertion a p title) = assertBoolVerbose title (f a @> p)
     in getList as
            |@>| mapAssertion
            @> sequence_
