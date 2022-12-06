{-# LANGUAGE LambdaCase #-}

module X.Utils.Value where

import Data.Number.CReal
import X.Data.LeftAssociativeInfixChain
import qualified X.Data.LeftAssociativeInfixChain as LAIL
import X.Data.Operator
import X.Data.Value
import X.Utils.LeftToRight

listToAdditiveChain :: [(AdditiveOperator, Value)] -> Value
listToAdditiveChain [] = Scalar 0
listToAdditiveChain ((Sub, x) : xs) = listToAdditiveChain $ (Add, Scalar 0) : (Sub, x) : xs
listToAdditiveChain ((Add, x) : xs) = AdditiveChain $ fromHeadAndList x xs

listToMultiplicativeChain :: [(MultiplicativeOperator, Value)] -> Value
listToMultiplicativeChain [] = Scalar 1
listToMultiplicativeChain ((Div, x) : xs) = listToMultiplicativeChain $ (Mul, Scalar 1) : (Div, x) : xs
listToMultiplicativeChain ((Mul, x) : xs) = MultiplicativeChain $ fromHeadAndList x xs

modifyAdditiveChainContents :: ([(AdditiveOperator, Value)] -> [(AdditiveOperator, Value)]) -> Value -> Value
modifyAdditiveChainContents f = \case
    AdditiveChain xs ->
        LAIL.toListWithInitialOperator Add xs
            @> f
            @> listToAdditiveChain
    x -> x

modifyMultiplicativeChainContents :: ([(MultiplicativeOperator, Value)] -> [(MultiplicativeOperator, Value)]) -> Value -> Value
modifyMultiplicativeChainContents f = \case
    MultiplicativeChain xs ->
        LAIL.toListWithInitialOperator Mul xs
            @> f
            @> listToMultiplicativeChain
    x -> x

getScalarsFromChain :: OperatorLike o => [(o, Value)] -> [(o, CReal)]
getScalarsFromChain =
    foldr
        ( \(op, val) result ->
            case val of
                Scalar n -> (op, n) : result
                _ -> result
        )
        []

