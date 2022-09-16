module TestUtils.Assertions.FunctionAssertion where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Collector
import X.Utils.LeftToRight

data FunctionAssertion a b = EqAssertion a b | PredicateAssertion a (b -> Bool)

shouldEvalAndSatisfy :: a -> (b -> Bool) -> Collector (FunctionAssertion a b) ()
shouldEvalAndSatisfy a f = PredicateAssertion a f @> singleton

shouldEvalTo :: a -> b -> Collector (FunctionAssertion a b) ()
shouldEvalTo a b = EqAssertion a b @> singleton

functionAssertion :: (Show b, Eq b) => (a -> b) -> Collector (FunctionAssertion a b) c -> Assertion
functionAssertion f as =
    let mapAssertion (EqAssertion a b) = assertEqual (f a) b
        mapAssertion (PredicateAssertion a p) = assertBool (f a @> p)
     in getList as
            |@>| mapAssertion
            @> sequence_
