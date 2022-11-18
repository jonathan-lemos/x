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
import TestUtils.DSL.Value

parseValue :: String -> Value
parseValue =
    parse additiveExpression
        |@>| right
        |@>| snd

simplifierWithConvergents :: Simplifier -> Simplifier
simplifierWithConvergents x = aggregateSimplifier $ x:convergentSimplifiers

simplifyThenOthers :: String -> (Value -> Value)
simplifyThenOthers t =
    let afterSimplify =
            simplifiers
                @> filter (simplifierName |@>| (/= t))
                @> aggregateSimplifier
                @> runSimplifier
        doSimplify =
            simplifiers
                @> filter (simplifierName |@>| (== t))
                @> head
                @> runSimplifier
     in doSimplify |@>| afterSimplify

shouldNotChange :: (DSLValueLike v) => v -> Collector (FunctionAssertion Value Value) ()
shouldNotChange v = dslToValue v `shouldEvalTo` dslToValue v

shouldSimplifyTo :: (DSLValueLike a, DSLValueLike b) => a -> b -> (Collector (FunctionAssertion Value Value)) ()
shouldSimplifyTo a b = dslToValue a `shouldEvalTo` dslToValue b
