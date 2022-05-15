module Types.AST.UnitExpression where

import Data.List
import Data.Number.CReal

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
