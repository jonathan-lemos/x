module X.Control.Parser.AST.Value.UnitFactorSpec where

import X.Control.Parser.AST.Value.UnitFactor
import Test.Hspec
import X.Data.AST.UnitExpression
import Harness.ParserCase
import X.Data.ParseError

spec :: Spec
spec = parallel $ do
    parserDesc unitFactor "unitFactor" $ do
        "kg" `shouldTotallyParseTo` JustUnit "kg"
        "kg^2" `shouldTotallyParseTo` UnitPower "kg" 2
        "N^2" `shouldTotallyParseTo` UnitPower "N" 2
        "N^-1" `shouldTotallyParseTo` UnitPower "N" (-1)
        "mol^-5" `shouldTotallyParseTo` UnitPower "mol" (-5)

        "kg*m" `shouldPartiallyParseTo` JustUnit "kg" `withRemainder` "*m"
        "kg + 4 m" `shouldPartiallyParseTo` JustUnit "kg" `withRemainder` " + 4 m"

        " foo" `shouldFailWith` ParseError "Expected an A-z character" " foo"
        "kg^" `shouldFailWith` ParseError "Expected a sequence of digits" ""
        "kg^ foo" `shouldFailWith` ParseError "Expected a sequence of digits" " foo"

