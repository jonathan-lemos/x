module Types.AST.Value.Value where

import Types.AST.Value.Scalar
import Types.AST.UnitExpression

data Value = Value Scalar (Maybe UnitExpression)
    deriving Eq

instance Show Value where
    show (Value sc ue) = show sc <> maybe "" ((" " <>) . show) ue
