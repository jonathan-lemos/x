module Parser.Parsers.AST.Value.UnitExpressionSpec where

import Parser.Parsers.AST.Value.UnitExpression
import Test.Hspec
import TestUtils.Parser
import Types.AST.UnitExpression

spec :: Spec
spec = parallel $ do
    passPartialFailSpec
        "unitMultExpr"
        unitMultExpr
        [ ("kg*m*s^-2", UnitMultExpression (JustUnit "kg") [JustUnit "m", UnitPower "s" (-2)])
        , ("kg", UnitMultExpression (JustUnit "kg") [])
        , ("kg^2", UnitMultExpression (UnitPower "kg" 2) [])
        , ("kg^2*m^2*N", UnitMultExpression (UnitPower "kg" 2) [UnitPower "m" 2, JustUnit "N"])
        ]
        [ ("kg*m^2 foo", UnitMultExpression (JustUnit "kg") [UnitPower "m" 2], " foo")
        , ("kg*m^2 ", UnitMultExpression (JustUnit "kg") [UnitPower "m" 2], " ")
        , ("kg*m^2_", UnitMultExpression (JustUnit "kg") [UnitPower "m" 2], "_")
        -- , ("kg* 3m", UnitMultExpression (JustUnit "kg") [], "* 3m")
        , ("kg *3m", UnitMultExpression (JustUnit "kg") [], " *3m")
        ]
        [ ("_foobar", "Expected an A-z character", "_foobar")
        , (" kg", "Expected an A-z character", " kg")
        , ("kg*", "Expected an A-z character", "")
        ]
