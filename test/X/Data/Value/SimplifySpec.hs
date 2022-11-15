{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Data.Value.SimplifySpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import X.Data.Value
import X.Data.Value.Simplifier
import X.Data.Value.Simplify
import X.TestUtils.Value
import X.Data.Operator
import X.TestUtils.Simplifier

test_reduceSingleElementChain :: Assertion
test_reduceSingleElementChain =
    functionAssertion (simplifyingFunction reduceSingleElementChain) $ do
        shouldNotChange (Scalar 1)
        shouldNotChange (Variable "foo")
        shouldNotChange (ExpChain (Scalar 1) (Scalar 2))
        shouldNotChange (ac (Scalar 1) [(Add, Scalar 2)])
        shouldNotChange (mc (Scalar 1) [(Mul, Scalar 2)])
        ac (Scalar 1) [] `shouldSimplifyTo` Scalar 1
        mc (Scalar 1) [] `shouldSimplifyTo` Scalar 1

test_toCoefficientAndValue :: Assertion
test_toCoefficientAndValue =
    functionAssertion toCoefficientAndValue $ do
        Scalar 1 `shouldEvalTo` (1, Scalar 1)
        Scalar 3 `shouldEvalTo` (3, Scalar 1)
        mc (Scalar 3) [(Mul, Variable "foo")] `shouldEvalTo` (3, mc (Variable "foo") [])
        mc (Scalar 3) [(Mul, Variable "foo"), (Mul, Variable "bar")] `shouldEvalTo` (3, mc (Variable "foo") [(Mul, Variable "bar")])
        mc (Variable "foo") [(Mul, Scalar 3), (Mul, Variable "bar")] `shouldEvalTo` (3, mc (Variable "foo") [(Mul, Variable "bar")])
        mc (Scalar 3) [(Mul, Scalar 5)] `shouldEvalTo` (15, Scalar 1)
        ac (Scalar 3) [(Add, Scalar 5)] `shouldEvalTo` (1, ac (Scalar 3) [(Add, Scalar 5)])

test_sumLikeTerms :: Assertion
test_sumLikeTerms =
    functionAssertion (simplifyingFunction $ simplifierWithConvergents sumLikeTerms) $ do
        shouldNotChange (Scalar 1)
        shouldNotChange (Variable "foo")
        shouldNotChange (ExpChain (Scalar 1) (Scalar 2))
        shouldNotChange (mc (Scalar 1) [(Mul, Scalar 2)])
        ac (Scalar 1) [(Add, Scalar 2)] `shouldSimplifyTo` mc (Scalar 3) [(Mul, Scalar 1)]
        ac (mc (Scalar 3) [(Mul, Variable "foo")])
            [(Add,
                mc (Scalar 2) [(Mul, Variable "foo")])]
                `shouldSimplifyTo`
                    mc (Scalar 5) [(Mul, Variable "foo")]
