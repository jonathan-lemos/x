module Parser.Parsers.AST.Value.ValueSpec where

import Test.Hspec
import TestUtils.Parser
import Parser.Parsers.AST.Value.Value
import Types.AST.Value.Scalar
import Types.AST.Value.Value
import Types.AST.UnitExpression

spec :: Spec
spec = do
    passPartialFailSpec "value" value
        [("2", Value (Number 2) Nothing)
        , ("-2.5", Value (Number (-2.5)) Nothing)
        , ("15 kg", Value (Number 15) (Just . UnitProduct $ UnitMultExpression (JustUnit "kg") [] ))
        , ("15kg", Value (Number 15) (Just . UnitProduct $ UnitMultExpression (JustUnit "kg") [] ))
        , ("15 kg*m^2/s^2*N", Value (Number 15) (Just (UnitFraction (UnitMultExpression (JustUnit "kg") [UnitPower "m" 2]) (UnitMultExpression (UnitPower "s" 2) [JustUnit "N"]))))]
        [("2 + 4", Value (Number 2) Nothing, " + 4")
        , ("15 kg + 5 kg", Value (Number 15) (Just . UnitProduct $ UnitMultExpression (JustUnit "kg") [] ), " + 5 kg")]
        [("+", "Expected a number or a variable name", "+")
        ,("", "Expected a number or a variable name", "")]
