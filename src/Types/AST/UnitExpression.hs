module Types.AST.UnitExpression where

import Data.List
import Data.Number.CReal
import Types.Unit.ValueUnit (ValueUnit)
import Types.Unit.Exponential
import Types.Unit.BaseUnit

data UnitFactor = UnitPower String CReal | JustUnit String
    deriving Eq

instance Show UnitFactor where
    show (UnitPower unit real) = show unit <> "^" <> show real
    show (JustUnit unit) = show unit

data UnitMultExpression = UnitMultExpression UnitFactor [UnitFactor]
    deriving Eq

instance Show UnitMultExpression where
    show (UnitMultExpression x xs) = intercalate "*" $ show <$> x:xs

data UnitExpression = UnitFraction UnitMultExpression UnitMultExpression | UnitProduct UnitMultExpression
    deriving Eq

instance Show UnitExpression where
    show (UnitFraction ua ub) = show ua <> "/" <> show ub
    show (UnitProduct e) = show e

unitExprToExps :: UnitExpression -> [Exponential String]
unitExprToExps =
    let ufToExp :: UnitFactor -> Exponential String
        ufToExp (UnitPower b e) = Exponential b e
        ufToExp (JustUnit b) = Exponential b 1

        umToExps :: UnitMultExpression -> [Exponential String]
        umToExps (UnitMultExpression x xs) = reduceExpProduct $ fmap ufToExp (x:xs)

        ueToExps :: UnitExpression -> [Exponential String]
        ueToExps (UnitFraction numerator denominator) = mergeExpProducts (umToExps numerator) (expComplement $ umToExps denominator)
        ueToExps (UnitProduct product) = umToExps product

        in ueToExps
