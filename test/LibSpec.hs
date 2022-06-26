module LibSpec where

import X.Control.Terminal
import Lib
import Test.Hspec
import X.TestUtils.MockTerminal
import X.TestUtils.Should (shouldBeSpec)
import X.TestUtils.List

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
