module Types.Value.Value where

import Types.Value.Scalar
import Unit.ValueUnit

data Value = Dimensionless Scalar | Dimensioned Scalar ValueUnit
    deriving Eq

instance Show Value where
    show (Dimensionless sc) = show sc
    show (Dimensioned sc vu) = show sc <> " " <> show vu
