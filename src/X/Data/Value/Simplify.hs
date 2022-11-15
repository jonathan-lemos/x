{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module X.Data.Value.Simplify where

import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as DM
import Data.Number.CReal
import X.Data.LeftAssociativeInfixChain
import qualified X.Data.LeftAssociativeInfixChain as LAIL
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplifier
import X.Utils.LeftToRight
import X.Utils.Map
import X.Utils.Operator
import X.Utils.Value

-- | Converts an additive/multiplicative chain of a single element into that element.
reduceSingleElementChain :: Simplifier
reduceSingleElementChain =
    Simplifier
        "single element chain"
        ( \case
            AdditiveChain (LAIL.Leaf x) -> x
            MultiplicativeChain (LAIL.Leaf x) -> x
            x -> x
        )
        True

toCoefficientAndValue :: Value -> (CReal, Value)
toCoefficientAndValue = \case
    MultiplicativeChain xs ->
        let content = LAIL.toListWithInitialOperator Mul xs
            coefficient =
                getScalarsFromChain content
                    @> foldl' (\ret (op, val) -> applyOp op ret val) 1
            value = modifyMultiplicativeChainContents (filter $ snd |@>| isScalar |@>| not) (MultiplicativeChain xs)
         in (coefficient, value)
    Scalar n -> (n, Scalar 1)
    x -> (1, x)

-- | Adds like terms e.g. (2x + 3x) -> 5x
sumLikeTerms :: Simplifier
sumLikeTerms =
    Simplifier
        "sum like terms"
        ( \case
            AdditiveChain xs ->
                LAIL.toListWithInitialOperator Add xs
                    |@>| second toCoefficientAndValue
                    @> groupMap (snd . snd) (\(op, (quantity, _val)) -> (op, quantity))
                    @> DM.assocs
                    @> foldl'
                        ( \result (val, xs) ->
                            MultiplicativeChain
                                (Link (Leaf . Scalar $ evalOperators xs 0) Mul val)
                                : result
                        )
                        []
                    |@>| (Add,)
                    @> listToAdditiveChain
            x -> x
        )
        False

simplifiers :: [Simplifier]
simplifiers =
    [ reduceSingleElementChain
    , sumLikeTerms
    ]

convergentSimplifiers :: [Simplifier]
convergentSimplifiers = filter convergent simplifiers

-- | Simplifies a value
simplify :: Value -> Value
simplify = aggregateSimplifier simplifiers @> runSimplifier
