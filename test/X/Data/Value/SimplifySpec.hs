{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Data.Value.SimplifySpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import TestUtils.Collector
import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import X.Data.Value
import X.Data.Value.Simplifier
import X.Data.Value.Simplify
import X.TestUtils.Either
import X.Utils.LeftToRight
import X.TestUtils.Value

parseValue :: String -> Value
parseValue =
    parse additiveExpression
        |@>| right
        |@>| snd

class ValueLike a where
    toValue :: a -> Value

instance ValueLike Value where
    toValue = id

instance ValueLike String where
    toValue = parseValue

simplifyThenOthers :: String -> (Value -> Value)
simplifyThenOthers t =
    let afterSimplify =
            simplifiers
                @> filter (simplifierName |@>| (/= t))
                @> aggregateSimplifier ""
                @> runSimplifier
        doSimplify =
            simplifiers
                @> filter (simplifierName |@>| (== t))
                @> head
                @> runSimplifier
     in doSimplify |@>| afterSimplify

shouldNotChange :: (ValueLike v) => v -> Collector (FunctionAssertion Value Value) ()
shouldNotChange v = toValue v `shouldEvalTo` toValue v

shouldSimplifyTo :: (ValueLike a, ValueLike b) => a -> b -> (Collector (FunctionAssertion Value Value)) ()
shouldSimplifyTo a b = toValue a `shouldEvalTo` toValue b

test_simplifySingleElementChain :: Assertion
test_simplifySingleElementChain =
    functionAssertion (runSimplifier simplifySingleElementChain) $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "1^2"
        shouldNotChange "1+2"
        shouldNotChange "1*2"
        ac (Scalar 1) [] `shouldSimplifyTo` Scalar 1
        mc (Scalar 1) [] `shouldSimplifyTo` Scalar 1

