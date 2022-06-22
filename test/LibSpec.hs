module LibSpec where

import IO.Terminal
import Lib
import Test.Hspec
import TestUtils.MockTerminal
import TestUtils.Should (shouldBeSpec)
import TestUtils.List

spec :: Spec
spec = do
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
                , "arithmetic"
                )
            ]

    let tcToSpecCase (input, dims, output, name) =
            (show <$> runPure input dims mainLoop, weave (replicate (length output + 1) "x> ") output, name)

    shouldBeSpec
        "end-to-end tests"
        $ tcToSpecCase <$> tcs
