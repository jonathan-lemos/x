module X.Data.Value where

import Data.List
import Data.Number.CReal
import X.Data.LeftAssociativeInfixList
import X.Data.Operator
import X.Utils.LeftToRight

data Value
    = Scalar CReal
    | Variable String
    | AdditiveChain (LeftAssociativeInfixList AdditiveOperator Value)
    | MultiplicativeChain (LeftAssociativeInfixList MultiplicativeOperator Value)
    | ExpChain Value Value
    deriving (Eq, Ord, Show)

displayValue :: Value -> String
displayValue (Scalar sc) = show sc
displayValue (Variable x) = show x
displayValue (LeftAssociativeChain ls) =
    let (x, xs) = toHeadAndList ls
     in xs
            |@>| (\(a, b) -> show a <> show b)
            @> intercalate ""
            @> (show x <>)
displayValue (RightAssociativeChain a op b) =
    show a <> show op <> show b
