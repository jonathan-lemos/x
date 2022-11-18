{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Data.Value.SimplifierSpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import TestUtils.DSL.Value
import X.Data.LeftAssociativeInfixChain
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplifier
import X.TestUtils.Simplifier

simpleSimplifier :: Simplifier
simpleSimplifier =
    Simplifier
        "simple"
        ( \case
            Scalar 1 -> Scalar 2
            x -> x
        )
        True

repeatedSimplifier :: Simplifier
repeatedSimplifier =
    Simplifier
        "repeated"
        ( \case
            Scalar x | x < 5 -> Scalar (x + 1)
            x -> x
        )
        True

postorderSimplifier :: Simplifier
postorderSimplifier =
    Simplifier
        "postorder"
        ( \case
            Scalar 1 -> Scalar 2
            AdditiveChain (Link (Leaf (Scalar 2)) Add (Scalar 3)) -> Scalar 5
            x -> x
        )
        True

test_simplifierRecurses :: Assertion
test_simplifierRecurses =
    functionAssertion (runSimplifier simpleSimplifier) $ do
        shouldNotChange $ sc 69
        shouldNotChange (sc 2 @+ sc 2)
        shouldNotChange (sc 2 @* sc 2)
        shouldNotChange (sc 2 @- sc 2)
        shouldNotChange (sc 2 @- sc 2)

        sc 1 `shouldSimplifyTo` sc 2
        (sc 1 @+ sc 3 @+ sc 1 @* sc 3 @* sc 1) `shouldSimplifyTo` (sc 2 @+ sc 3 @+ sc 2 @* sc 3 @* sc 2)

test_simplifierRepeats :: Assertion
test_simplifierRepeats =
    functionAssertion (runSimplifier repeatedSimplifier) $ do
        shouldNotChange $ sc 69
        shouldNotChange (sc 6 @+ sc 6)
        shouldNotChange (sc 5 @* sc 5)
        shouldNotChange (sc 5 @- sc 5)
        shouldNotChange (sc 5 @/ sc 5)

        sc 1 `shouldSimplifyTo` sc 5
        (sc 1 @+ sc 3 @+ sc 1 @* sc 8 @* sc 1) `shouldSimplifyTo` (sc 5 @+ sc 5 @+ sc 5 @* sc 8 @* sc 5)

test_simplifierRunsPostorder :: Assertion
test_simplifierRunsPostorder =
    functionAssertion (runSimplifier postorderSimplifier) $ do
        shouldNotChange $ sc 69
        shouldNotChange $ sc 6 @+ sc 6
        shouldNotChange $ sc 5 @* sc 5
        shouldNotChange $ sc 5 @- sc 5
        shouldNotChange $ sc 5 @/ sc 5

        (sc 1 @+ sc 1) `shouldSimplifyTo` (sc 2 @+ sc 2)
        (sc 1 @+ sc 3) `shouldSimplifyTo` sc 5

