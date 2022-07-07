module X.Control.Parser.AST.Value.UnitExpressionSpec where

import X.Control.Parser.AST.Value.UnitExpression
import Test.Hspec
import X.Data.AST.UnitExpression
import Harness.ParserCase
import X.Data.ParseError
import Harness.With

spec :: Spec
spec = parallel $ do
    parserDesc unitMultExpr "unitMultExpr" $ do
        "kg*m*s^-2" `shouldParseTo` UnitMultExpression (JustUnit "kg") [JustUnit "m", UnitPower "s" (-2)]
        "kg" `shouldParseTo` UnitMultExpression (JustUnit "kg") []
        "kg*m*s^-2" `shouldParseTo` UnitMultExpression (JustUnit "kg") [JustUnit "m", UnitPower "s" (-2)]
        "kg" `shouldParseTo` UnitMultExpression (JustUnit "kg") []
        "kg^2" `shouldParseTo` UnitMultExpression (UnitPower "kg" 2) []
        "kg^2*m^2*N" `shouldParseTo` UnitMultExpression (UnitPower "kg" 2) [UnitPower "m" 2, JustUnit "N"]

        "kg*m^2 foo" `shouldParseTo` UnitMultExpression (JustUnit "kg") [UnitPower "m" 2] `withRemainder` " foo"
        "kg*m^2 " `shouldParseTo` UnitMultExpression (JustUnit "kg") [UnitPower "m" 2] `withRemainder` " "
        "kg*m^2_" `shouldParseTo` UnitMultExpression (JustUnit "kg") [UnitPower "m" 2] `withRemainder` "_"
        -- , ("kg* 3m", UnitMultExpression (JustUnit "kg") [], "* 3m")
        "kg *3m" `shouldParseTo` UnitMultExpression (JustUnit "kg") [] `withRemainder` " *3m"

        "_foobar" `shouldFailWithReason` "Expected an A-z character" `andRemainder` "_foobar"
        " kg" `shouldFailWithReason` "Expected an A-z character" `andRemainder` " kg"
        "kg*" `shouldFailWithReason` "Expected an A-z character" `andRemainder` ""
