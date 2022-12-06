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

test_simplifyMultiplyBy1 :: Assertion
test_simplifyMultiplyBy1 =
    functionAssertion (simplifyingFunction simplifyMultiplyBy1) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 1 @^ sc 2)
        shouldNotChange (sc 1 @+ sc 2)

        (sc 1 @* "foo") `shouldSimplifyTo` mc "foo"
        (sc 3 @* sc 1 @* "foo") `shouldSimplifyTo` (sc 3 @* "foo")
        (sc 3 @* "bar" @* sc 1 @* "foo") `shouldSimplifyTo` (sc 3 @* "bar" @* "foo")

test_simplifyExpBy1 :: Assertion
test_simplifyExpBy1 =
    functionAssertion (simplifyingFunction simplifyExpBy1) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 1 @^ sc 2)
        shouldNotChange (sc 1 @+ sc 2)

        ("foo" @^ sc 1) `shouldSimplifyTo` "foo"
        ((sc 3 @* sc 1) @^ sc 1) `shouldSimplifyTo` (sc 3 @* sc 1)

test_simplifyExp1ByAnything :: Assertion
test_simplifyExp1ByAnything =
    functionAssertion (simplifyingFunction simplifyExp1ByAnything) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 2 @^ sc 1)
        shouldNotChange (sc 1 @+ sc 2)

        (sc 1 @^ "foo") `shouldSimplifyTo` sc 1
        (sc 1 @^ sc 2) `shouldSimplifyTo` sc 1

test_performScalarExponentiation :: Assertion
test_performScalarExponentiation =
    functionAssertion (simplifyingFunction performScalarExponentiation) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 1 @+ sc 2)
        shouldNotChange (sc 1 @* sc 2)

        (sc 3 @^ sc 2) `shouldSimplifyTo` sc 9
        (sc 2 @^ sc 1) `shouldSimplifyTo` sc 2

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
        shouldNotChange (sc 2 @^ "foo")
        shouldNotChange (sc 2 @* sc 3)
        (sc 1 @+ sc 2) `shouldSimplifyTo` sc 3
        (sc 3 @* "foo" @+ sc 2 @* "foo") `shouldSimplifyTo` (sc 5 @* "foo")
        ("foo" @* sc 3 @+ sc 2 @* "foo") `shouldSimplifyTo` (sc 5 @* "foo")
        (sc 1 @- sc 2) `shouldSimplifyTo` sc (-1)

test_multiplyLikeTerms :: Assertion
test_multiplyLikeTerms =
    functionAssertion (simplifyingFunction $ simplifierWithConvergents multiplyLikeTerms) $ do
        shouldNotChange (sc 1)
        shouldNotChange "foo"
        shouldNotChange (sc 2 @^ "foo")
        shouldNotChange (sc 3 @+ sc 2)
        (sc 3 @* "foo" @* "foo" @* "bar" @* sc 2) `shouldSimplifyTo` (sc 2 @* sc 3 @* "bar" @* "foo" @^ (sc 1 @+ sc 1))
        (sc 3 @* "foo" @^ sc 2 @* "foo" @^ sc 3) `shouldSimplifyTo` (sc 3 @* "foo" @^ (sc 2 @+ sc 3))
        ("foo" @^ sc 3 @/ "foo") `shouldSimplifyTo` ("foo" @^ (sc 3 @- sc 1))
        ("foo" @^ sc 3 @/ "foo" @^ sc 2) `shouldSimplifyTo` ("foo" @^ (sc 3 @- sc 2))
        (sc 6 @^ "foo" @/ sc 6) `shouldSimplifyTo` (sc 6 @^ ("foo" @- sc 1))
        ("foo" @^ "bar" @* "foo" @^ "baz") `shouldSimplifyTo` ("foo" @^ ("bar" @+ "baz"))
