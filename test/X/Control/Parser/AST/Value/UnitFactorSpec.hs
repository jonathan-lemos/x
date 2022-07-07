module X.Control.Parser.AST.Value.UnitFactorSpec where

import X.Control.Parser.AST.Value.UnitFactor
import Test.Hspec
import X.Data.AST.UnitExpression
import Harness.ParserCase
import X.Data.ParseError
import Harness.With

spec :: Spec
spec = parallel $ do
    parserDesc unitFactor "unitFactor" $ do
        "kg" `shouldParseTo` JustUnit "kg"
        "kg^2" `shouldParseTo` UnitPower "kg" 2
        "N^2" `shouldParseTo` UnitPower "N" 2
        "N^-1" `shouldParseTo` UnitPower "N" (-1)
        "mol^-5" `shouldParseTo` UnitPower "mol" (-5)

        "kg*m" `shouldParseTo` JustUnit "kg" `withRemainder` "*m"
        "kg + 4 m" `shouldParseTo` JustUnit "kg" `withRemainder` " + 4 m"

        " foo" `shouldFailWithReason` "Expected an A-z character" `andRemainder` " foo"
        "kg^" `shouldFailWithReason` "Expected a sequence of digits" `andRemainder` ""
        "kg^ foo" `shouldFailWithReason` "Expected a sequence of digits" `andRemainder` " foo"

