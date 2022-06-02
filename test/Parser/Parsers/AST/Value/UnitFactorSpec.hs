module Parser.Parsers.AST.Value.UnitFactorSpec where

import Parser.Parsers.AST.Value.UnitFactor
import Test.Hspec
import TestUtils.Parser
import Types.AST.UnitExpression

spec :: Spec
spec = parallel $ do
    passPartialFailSpec
        "unitFactor"
        unitFactor
        [ ("kg", JustUnit "kg")
        , ("kg^2", UnitPower "kg" 2)
        , ("N^2", UnitPower "N" 2)
        , ("N^-1", UnitPower "N" (-1))
        , ("mol^-5", UnitPower "mol" (-5))
        ]
        [ ("kg*m", JustUnit "kg", "*m")
        , ("kg + 4 m", JustUnit "kg", " + 4 m")
        ]
        [ (" foo", "Expected an A-z character", " foo")
        , ("kg^", "Expected a sequence of digits", "")
        , ("kg^ foo", "Expected a sequence of digits", " foo")
        ]
