module X.Data.Value where

import Data.Number.CReal
import X.Data.Operator
import X.Data.LeftAssociativeInfixChain

data Value
    = Scalar CReal
    | Variable String
    | AdditiveChain (LeftAssociativeInfixChain AdditiveOperator Value)
    | MultiplicativeChain (LeftAssociativeInfixChain MultiplicativeOperator Value)
    | ExpChain Value Value
    deriving (Eq, Ord, Show)

displayValue :: Value -> String
displayValue = show

