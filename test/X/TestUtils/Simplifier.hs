{-# LANGUAGE FlexibleInstances #-}

module X.TestUtils.Simplifier where

import TestUtils.Assertions.FunctionAssertion
import TestUtils.Collector
import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import X.Data.Value
import X.Data.Value.Simplifier
import X.Data.Value.Simplify
import X.TestUtils.Either
import X.Utils.LeftToRight

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
