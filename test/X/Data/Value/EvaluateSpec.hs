{-# OPTIONS_GHC -F -pgmF htfpp #-}

module X.Data.Value.EvaluateSpec where

import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.FunctionAssertion
import TestUtils.DSL.Value
import X.Data.Value.Evaluate
import X.TestUtils.Context
import X.TestUtils.Simplifier

test_evaluateValue :: Assertion
test_evaluateValue =
    let testContext =
            mkCtx
                [ ("num", sc 4)
                , ("expr", "x" @+ sc 2)
                ]
     in functionAssertion (`evaluateValue` testContext) $ do
            shouldNotChange $ sc 2 @+ "x"
            shouldNotChange $ sc 2 @- "x"

