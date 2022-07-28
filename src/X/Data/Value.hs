module X.Data.Value where

import Data.Number.CReal
import X.Data.Operator
import X.Utils.LeftToRight

data Value
    = Scalar CReal
    | Variable String
    | AdditiveChain Value [(AdditiveOperator, Value)]
    | MultiplicativeChain Value [(MultiplicativeOperator, Value)]
    | ExpChain Value Value
    deriving (Eq, Ord)

transformAdditiveChain :: ([(AdditiveOperator, Value)] -> Value) -> Value -> Value
transformAdditiveChain f (AdditiveChain x xs) = (Add, x) : xs @> f
transformAdditiveChain _ v = v

transformMultiplicativeChain :: ([(MultiplicativeOperator, Value)] -> Value) -> Value -> Value
transformMultiplicativeChain f (MultiplicativeChain x xs) = (Mul, x) : xs @> f
transformMultiplicativeChain _ v = v

additiveChainFromList :: [(AdditiveOperator, Value)] -> Value
additiveChainFromList ((opHead, valueHead) : tail) =
    let newHead = case opHead of
            Add -> valueHead
            Sub -> MultiplicativeChain (Scalar (-1)) [(Mul, valueHead)]
     in AdditiveChain newHead tail
additiveChainFromList [] = Scalar 0

multiplicativeChainFromList :: [(MultiplicativeOperator, Value)] -> Value
multiplicativeChainFromList ((opHead, valueHead) : tail) =
    let newHead = case opHead of
            Mul -> valueHead
            Div -> MultiplicativeChain (Scalar 1) [(Div, valueHead)]
     in MultiplicativeChain newHead tail
multiplicativeChainFromList [] = Scalar 1

mapAdditiveChain :: ([(AdditiveOperator, Value)] -> [(AdditiveOperator, Value)]) -> Value -> Value
mapAdditiveChain f = transformAdditiveChain (f |@>| additiveChainFromList)

mapMultiplicativeChain :: ([(MultiplicativeOperator, Value)] -> [(MultiplicativeOperator, Value)]) -> Value -> Value
mapMultiplicativeChain f = transformMultiplicativeChain (f |@>| multiplicativeChainFromList)

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
