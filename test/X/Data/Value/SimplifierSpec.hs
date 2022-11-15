{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Data.Value.SimplifierSpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import X.Data.LeftAssociativeInfixChain
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplifier
import X.TestUtils.Simplifier
import X.TestUtils.Value

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
        shouldNotChange "69"
        shouldNotChange "2+2"
        shouldNotChange "2*2"
        shouldNotChange "2-2"
        shouldNotChange "2-2"

        "1" `shouldSimplifyTo` "2"
        "1+3+1*3*1" `shouldSimplifyTo` "2+3+2*3*2"

test_simplifierRepeats :: Assertion
test_simplifierRepeats =
    functionAssertion (runSimplifier repeatedSimplifier) $ do
        shouldNotChange "69"
        shouldNotChange "6+6"
        shouldNotChange "5*5"
        shouldNotChange "5-5"
        shouldNotChange "5/5"

        "1" `shouldSimplifyTo` "5"
        "1+3+1*8*1" `shouldSimplifyTo` "5+5+5*8*5"

test_simplifierRunsPostorder :: Assertion
test_simplifierRunsPostorder =
    functionAssertion (runSimplifier postorderSimplifier) $ do
        shouldNotChange "69"
        shouldNotChange "6+6"
        shouldNotChange "5*5"
        shouldNotChange "5-5"
        shouldNotChange "5/5"

        "1+1" `shouldSimplifyTo` "2+2"
        ac (Scalar 1) [(Add, Scalar 3)] `shouldSimplifyTo` Scalar 5
