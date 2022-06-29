module X.Control.Parser.Combinator.Branch.CheckSpec where

import Test.Hspec
import X.TestUtils.Parser
import X.Utils.Function
import X.Control.Parser.Combinator.Branch.Check

spec :: Spec
spec = do
    let sampleParser = check (length |> (< 5)) (<> " failed") parser

    let dup a = (a, a)

    let totalCases =
            dup
                <$> [ "foo"
                    , "bar"
                    , "baz"
                    ]

    let dupWith (a, b) = (a, a, b)

    let partialCases =
            dupWith
                <$> [ ("foo bar", " bar")
                    , ("baz_", "_")
                    ]

    let failCases =
                [ ("abcde", "abcde failed", "abcde")
                , ("_", "Expected an A-z character", "_")
                , ("", "Expected an A-z character", "")
                ]

    passPartialFailSpec "check" sampleParser totalCases partialCases failCases
