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

test_simplifySingleElementChain :: Assertion
test_simplifySingleElementChain =
    functionAssertion (runSimplifier simplifySingleElementChain) $ do
        shouldNotChange (Scalar 1)
        shouldNotChange (Variable "foo")
        shouldNotChange (ExpChain (Scalar 1) (Scalar 2))
        shouldNotChange (ac (Scalar 1) [(Add, Scalar 2)])
        shouldNotChange (mc (Scalar 1) [(Mul, Scalar 2)])
        ac (Scalar 1) [] `shouldSimplifyTo` Scalar 1
        mc (Scalar 1) [] `shouldSimplifyTo` Scalar 1

