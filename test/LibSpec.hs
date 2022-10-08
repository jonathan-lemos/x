{-# OPTIONS_GHC -F -pgmF htfpp #-}
module LibSpec where

import Lib
import Test.Framework
import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion
import X.TestUtils.List
import X.TestUtils.MockTerminal
import X.Utils.LeftToRight

test_endToEnd :: Assertion
test_endToEnd = do
    let tcs =
            [
                (
                    [ "f = 2 + 3"
                    , "f + 7"
                    ]
                , []
                ,
                    [ "f <- 5.0\n"
                    , "12.0\n"
                    ]
                )
            ]

    let tcToSpecCase (input, dims, output) =
            (runPure input dims mainLoop |@>| show) `shouldBe` weave (replicate (length output + 1) "x> ") output

    tcs
        |@>| tcToSpecCase
        @> sequence_
        @> basicAssertion
