{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE FlexibleInstances #-}

module X.Data.Value.SimplifySpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import TestUtils.Collector
import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import X.Data.Operator
import X.Data.Value
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

shouldNotChange :: (ValueLike v) => v -> Collector (FunctionAssertion Value Value) ()
shouldNotChange v = (toValue v) `shouldEvalTo` (toValue v)

shouldSimplifyTo :: (ValueLike a, ValueLike b) => a -> b -> (Collector (FunctionAssertion Value Value)) ()
shouldSimplifyTo a b = (toValue a) `shouldEvalTo` (toValue b)

test_simplifySingleElementChain :: Assertion
test_simplifySingleElementChain =
    functionAssertion simplifySingleElementChain $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "1^2"
        shouldNotChange "1+2"
        shouldNotChange "1*2"
        AdditiveChain (Scalar 1) [] `shouldSimplifyTo` Scalar 1
        MultiplicativeChain (Scalar 1) [] `shouldSimplifyTo` Scalar 1

test_simplifyExponentiationBy0Or1 :: Assertion
test_simplifyExponentiationBy0Or1 =
    functionAssertion simplifyExponentiationBy0Or1 $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "foo^2"
        shouldNotChange "foo+2"
        shouldNotChange "foo*2"

        "foo^1" `shouldSimplifyTo` "foo"
        "foo^0" `shouldSimplifyTo` "1"
        "5^1" `shouldSimplifyTo` "5"

test_simplifyMultiplyBy0 :: Assertion
test_simplifyMultiplyBy0 =
    functionAssertion simplifyMultiplyBy0 $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "1^2"
        shouldNotChange "1+2"
        shouldNotChange "1*2"

        MultiplicativeChain (Scalar 0) [] `shouldSimplifyTo` Scalar 0
        MultiplicativeChain (Scalar 0) [(Mul, Variable "foo")] `shouldSimplifyTo` Scalar 0
        MultiplicativeChain (Variable "foo") [(Mul, Scalar 0)] `shouldSimplifyTo` Scalar 0

test_simplifyAdd0 :: Assertion
test_simplifyAdd0 =
    functionAssertion simplifyAdd0 $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "1^2"
        shouldNotChange "1*2"
        shouldNotChange "1+2"

        AdditiveChain (Scalar 0) [] `shouldSimplifyTo` Scalar 0
        AdditiveChain (Scalar 0) [(Add, Scalar 5)] `shouldSimplifyTo` AdditiveChain (Scalar 5) []
        AdditiveChain (Scalar 0) [(Add, Scalar 5), (Sub, Variable "x")] `shouldSimplifyTo` AdditiveChain (Scalar 5) [(Sub, Variable "x")]

test_simplifyMultiply1 :: Assertion
test_simplifyMultiply1 =
    functionAssertion simplifyMultiply1 $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "2*3"
        shouldNotChange "2+3"
        shouldNotChange "2^3"

        "1*5" `shouldSimplifyTo` "5"
        "5*1" `shouldSimplifyTo` "5"
        "1*1*5*1" `shouldSimplifyTo` "5"
        "1*5x" `shouldSimplifyTo` "5x"

test_simplifySortChainTerms :: Assertion
test_simplifySortChainTerms =
    functionAssertion simplifySortChainTerms $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "3*foo"
        shouldNotChange "foo+3"

        "foo*3" `shouldSimplifyTo` "3*foo"
        "3+foo" `shouldSimplifyTo` "foo+3"

test_simplifyLikeAdditiveTerms :: Assertion
test_simplifyLikeAdditiveTerms =
    functionAssertion simplifyLikeAdditiveTerms $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "3+2"
        shouldNotChange "3*2"
        shouldNotChange "3^2"

        "3+2" `shouldSimplifyTo` "5"
        "3-2" `shouldSimplifyTo` "1"
        "3*x+2*x" `shouldSimplifyTo` "5*x"
        "3*x-2*x" `shouldSimplifyTo` "1*x"
        "3+2+3*x+2*x" `shouldSimplifyTo` "5+5*x"

test_simplifyLikeMultiplicativeTerms :: Assertion
test_simplifyLikeMultiplicativeTerms =
    functionAssertion simplifyLikeAdditiveTerms $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "3+2"
        shouldNotChange "3*2"
        shouldNotChange "3^2"

        "3*2" `shouldSimplifyTo` "6"
        "6/2" `shouldSimplifyTo` "3"
        "3*x*2" `shouldSimplifyTo` "6*x"
        "3*x*2*y" `shouldSimplifyTo` "6*x*y"

test_simplifyLikeExpTerms :: Assertion
test_simplifyLikeExpTerms =
    functionAssertion simplifyLikeExpTerms $ do
        shouldNotChange "1"
        shouldNotChange "foo"
        shouldNotChange "3+2"
        shouldNotChange "3*2"

        "3^2" `shouldSimplifyTo` "9"
        "3*x*x" `shouldSimplifyTo` "3*x^2"
