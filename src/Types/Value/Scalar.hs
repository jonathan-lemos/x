module Types.Value.Scalar where

import Data.Number.CReal

data Scalar = Number CReal | Variable String
    deriving Eq


instance Show Scalar where
    show (Number n) = show n
    show (Variable v) = v
