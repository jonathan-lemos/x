{-# LANGUAGE LambdaCase #-}

module X.Utils.Value where

import Data.Number.CReal
import X.Data.LeftAssociativeInfixChain
import qualified X.Data.LeftAssociativeInfixChain as LAIL
import X.Data.Operator
import X.Data.Value
import X.Utils.LeftToRight
import X.Utils.CReal

listToAdditiveChain :: [(AdditiveOperator, Value)] -> Value
listToAdditiveChain [] = Scalar 0
listToAdditiveChain ((Sub, x) : xs) = listToAdditiveChain $ (Add, negateVal x) : xs
listToAdditiveChain ((Add, x) : xs) = AdditiveChain $ fromHeadAndList x xs

listToMultiplicativeChain :: [(MultiplicativeOperator, Value)] -> Value
listToMultiplicativeChain [] = Scalar 1
listToMultiplicativeChain ((Div, x) : xs) = listToMultiplicativeChain $ (Mul, reciprocateVal x) : xs
listToMultiplicativeChain ((Mul, x) : xs) = MultiplicativeChain $ fromHeadAndList x xs

negateVal :: Value -> Value
negateVal = \case
    Scalar sc -> Scalar (-sc)
    AdditiveChain ac ->
        let (head, tail) = LAIL.toHeadAndList ac
         in AdditiveChain $
                LAIL.fromHeadAndList
                    (negateVal head)
                    ( tail
                        |@>| ( \case
                                (Add, x) -> (Sub, x)
                                (Sub, x) -> (Add, x)
                             )
                    )
    MultiplicativeChain mc ->
        let (head, tail) = LAIL.toHeadAndList mc
         in MultiplicativeChain $
                LAIL.fromHeadAndList
                    (negateVal head)
                    tail
    x ->
        MultiplicativeChain (Link (Leaf (Scalar (-1))) Mul x)

reciprocateVal :: Value -> Value
reciprocateVal = \case
    Scalar 0 -> Scalar 0
    Scalar sc -> Scalar (1 `safeDiv` sc)
    MultiplicativeChain mc ->
        let (head, tail) = LAIL.toHeadAndList mc
         in MultiplicativeChain $
             LAIL.fromHeadAndList
                (reciprocateVal head)
                tail
    x -> MultiplicativeChain (Link (Leaf (Scalar 1)) Div x)

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
