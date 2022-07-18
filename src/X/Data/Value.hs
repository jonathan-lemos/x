module X.Data.Value where

import Data.Number.CReal
import X.Data.Operator
import Text.Printf

data Value
    = Scalar CReal
    | Variable String
    | InfixCall Value Operator Value
    deriving Eq

instance Show Value where
    show (Scalar sc) = show sc
    show (Variable v) = v
    show (InfixCall a op b) = printf "%s %s %s" (show a) (show op) (show b)
