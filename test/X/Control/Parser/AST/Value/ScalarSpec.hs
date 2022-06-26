module X.Control.Parser.AST.Value.ScalarSpec where

import X.Control.Parser.AST.Value.Scalar
import Test.Hspec
import X.TestUtils.Parser
import X.Data.AST.Token.Scalar

spec :: Spec
spec = do
    passPartialFailSpec
        "scalar"
        scalar
        [ ("2e4", Number 2e4)
        , ("-2.4", Number (-2.4))
        , ("2", Number 2)
        , ("foo", Variable "foo")
        , ("g", Variable "g")
        ]
        [ ("2e4 + 5", Number 2e4, " + 5")
        , ("foo * 7", Variable "foo", " * 7")]
        [ ("_", "Expected a number or a variable name", "_")
        , ("", "Expected a number or a variable name", "")
        , (" foo", "Expected a number or a variable name", " foo")
        ]
