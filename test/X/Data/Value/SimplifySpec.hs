{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Data.Value.SimplifySpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import X.Data.Value.Simplifier
import X.Data.Value.Simplify
import X.TestUtils.Simplifier
import TestUtils.DSL.Value

test_reduceSingleElementChain :: Assertion
test_reduceSingleElementChain =
    functionAssertion (simplifyingFunction reduceSingleElementChain) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 1 @^ sc 2)
        shouldNotChange (sc 1 @+ sc 2)
        shouldNotChange (sc 1 @* sc 2)
        ac (sc 1) `shouldSimplifyTo` sc 1
        mc (sc 1) `shouldSimplifyTo` sc 1

test_toCoefficientAndValue :: Assertion
test_toCoefficientAndValue =
    functionAssertion toCoefficientAndValue $ do
        sc 1 `shouldEvalTo` (1, sc 1)
        sc 3 `shouldEvalTo` (3, sc 1)
        (sc 3 @* "foo") `shouldEvalTo` (3, mc "foo")
        (sc 3 @* "foo") `shouldEvalTo` (3, mc "foo")
        (sc 3 @* "foo" @* "bar") `shouldEvalTo` (3, "foo" @* "bar")
        ("foo" @* sc 3 @* "bar") `shouldEvalTo` (3, "foo" @* "bar")
        (sc 3 @* sc 5) `shouldEvalTo` (15, sc 1)
        (sc 3 @+ sc 5) `shouldEvalTo` (1, sc 3 @+ sc 5)

test_sumLikeTerms :: Assertion
test_sumLikeTerms =
    functionAssertion (simplifyingFunction $ simplifierWithConvergents sumLikeTerms) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 1 @^ sc 2)
        shouldNotChange (sc 1 @* sc 2)
        (sc 1 @+ sc 2) `shouldSimplifyTo` (sc 3 @* sc 1)
        (sc 3 @* "foo" @+ sc 2 @* "foo") `shouldSimplifyTo` (sc 5 @* "foo")
        ("foo" @* sc 3 @+ sc 2 @* "foo") `shouldSimplifyTo` (sc 5 @* "foo")
