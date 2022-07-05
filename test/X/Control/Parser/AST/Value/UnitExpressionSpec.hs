module X.Control.Parser.AST.Value.UnitExpressionSpec where

import X.Control.Parser.AST.Value.UnitExpression
import Test.Hspec
import X.Data.AST.UnitExpression
import Harness.ParserCase
import X.Data.ParseError

spec :: Spec
spec = parallel $ do
    parserDesc unitMultExpr "unitMultExpr" $ do
        "kg*m*s^-2" `shouldTotallyParseTo` UnitMultExpression (JustUnit "kg") [JustUnit "m", UnitPower "s" (-2)]
        "kg" `shouldTotallyParseTo` UnitMultExpression (JustUnit "kg") []
        "kg*m*s^-2" `shouldTotallyParseTo` UnitMultExpression (JustUnit "kg") [JustUnit "m", UnitPower "s" (-2)]
        "kg" `shouldTotallyParseTo` UnitMultExpression (JustUnit "kg") []
        "kg^2" `shouldTotallyParseTo` UnitMultExpression (UnitPower "kg" 2) []
        "kg^2*m^2*N" `shouldTotallyParseTo` UnitMultExpression (UnitPower "kg" 2) [UnitPower "m" 2, JustUnit "N"]

        "kg*m^2 foo" `shouldPartiallyParseTo` UnitMultExpression (JustUnit "kg") [UnitPower "m" 2] `withRemainder` " foo"
        "kg*m^2 " `shouldPartiallyParseTo` UnitMultExpression (JustUnit "kg") [UnitPower "m" 2] `withRemainder` " "
        "kg*m^2_" `shouldPartiallyParseTo` UnitMultExpression (JustUnit "kg") [UnitPower "m" 2] `withRemainder` "_"
        -- , ("kg* 3m", UnitMultExpression (JustUnit "kg") [], "* 3m")
        "kg *3m" `shouldPartiallyParseTo` UnitMultExpression (JustUnit "kg") [] `withRemainder` " *3m"

        "_foobar" `shouldFailWith` ParseError "Expected an A-z character" "_foobar"
        " kg" `shouldFailWith` ParseError "Expected an A-z character" " kg"
        "kg*" `shouldFailWith` ParseError "Expected an A-z character" ""
