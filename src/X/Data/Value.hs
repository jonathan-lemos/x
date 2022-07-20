module X.Data.Value where

import Data.Number.CReal
import Text.Printf
import X.Data.Operator
import X.Utils.LeftToRight

data Value
    = Scalar CReal
    | Variable String
    | AdditiveChain Value [(AdditiveOperator, Value)]
    | MultiplicativeChain Value [(MultiplicativeOperator, Value)]
    | ExpChain Value Value
    deriving (Eq)

_showLeftAssociativeChain :: (Show a, Show b, Show c) => a -> [(b, c)] -> String
_showLeftAssociativeChain x xs =
        xs
            |@>| (\(op, v) -> show op <> " " <> show v)
            @> mconcat
            @> (show x <>)

instance Show Value where
    show (Scalar sc) = show sc
    show (Variable v) = v
    show (AdditiveChain x xs) = _showLeftAssociativeChain x xs
    show (MultiplicativeChain x xs) = _showLeftAssociativeChain x xs
    show (ExpChain b e) = show b <> "^" <> show e
