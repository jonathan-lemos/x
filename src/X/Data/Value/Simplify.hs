{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module X.Data.Value.Simplify where

import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as DM
import Data.Maybe
import Data.Number.CReal
import X.Data.LeftAssociativeInfixChain
import qualified X.Data.LeftAssociativeInfixChain as LAIL
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplifier
import X.Utils.CReal
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

-- | Turns 1*x into x
simplifyMultiplyBy1 :: Simplifier
simplifyMultiplyBy1 =
    Simplifier
        "multiplying by 1"
        (modifyMultiplicativeChainContents (filter $ snd |@>| (/= Scalar 1)))
        True

-- | Turns x^1 into x
simplifyExpBy1 :: Simplifier
simplifyExpBy1 =
    Simplifier
        "exponentiating by 1"
        ( \case
            ExpChain x (Scalar 1) -> x
            x -> x
        )
        True

-- | Turns 1^x into 1
simplifyExp1ByAnything :: Simplifier
simplifyExp1ByAnything =
    Simplifier
        "exponentiating 1^x"
        ( \case
            ExpChain (Scalar 1) _ -> Scalar 1
            x -> x
        )
        True

-- | Executes a^b if a and b are both Scalar
performScalarExponentiation :: Simplifier
performScalarExponentiation =
    Simplifier
        "exponentiating two scalars"
        ( \case
            ExpChain (Scalar a) (Scalar b) -> Scalar (a `safeExp` b)
            x -> x
        )
        True

-- | + (-5) -> - 5 and - (-5) to +5
simplifyDoubleNegative :: Simplifier
simplifyDoubleNegative =
    Simplifier
        "double negative"
        ( modifyAdditiveChainContents $
            fmap
                ( \case
                    (Add, Scalar n) | n < 0 -> (Sub, Scalar (-n))
                    (Sub, Scalar n) | n < 0 -> (Add, Scalar (-n))
                    x -> x
                )
        )
        True

simplifyAdditiveTimesNeg1 :: Simplifier
simplifyAdditiveTimesNeg1 =
    let n1count mc =
            count (LAIL.toList mc |@>| snd)
                @> DM.lookup (Scalar (-1))
                @> fromMaybe (0 :: Integer)
        removeN1s mc =
            listToMultiplicativeChain $
                filter
                    (snd |@>| (/= Scalar (-1)))
                    (LAIL.toList mc)
     in Simplifier
            "- (-1 * x)"
            ( modifyAdditiveChainContents $
                fmap
                    ( \case
                        (Sub, MultiplicativeChain mc) ->
                            if n1count mc `rem` 2 == 1
                                then (Add, removeN1s mc)
                                else (Sub, MultiplicativeChain mc)
                        (Add, MultiplicativeChain mc) ->
                            if n1count mc `rem` 2 == 1
                                then (Sub, removeN1s mc)
                                else (Add, MultiplicativeChain mc)
                        x -> x
                    )
            )
            True

-- | x+0 -> x
simplifyAdd0 :: Simplifier
simplifyAdd0 =
    Simplifier
        "add 0"
        (modifyAdditiveChainContents $ filter (snd |@>| (/= Scalar 0)))
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

coefficientMap :: (OperatorLike op, ChainHeadOp op) => LAIL.LeftAssociativeInfixChain op Value -> DM.Map Value [(op, CReal)]
coefficientMap xs =
    LAIL.toList xs
        |@>| second toCoefficientAndValue
        @> groupMap (snd . snd) (\(op, (quantity, _val)) -> (op, quantity))

-- | Adds like terms e.g. (2x + 3x) -> 5x
sumLikeTerms :: Simplifier
sumLikeTerms =
    Simplifier
        "sum like terms"
        ( \case
            AdditiveChain xs ->
                coefficientMap xs
                    |@>| evalOperators 0
                    @> DM.assocs
                    |@>| (\(val, num) -> MultiplicativeChain (Link (Leaf $ Scalar num) Mul val))
                    |@>| (Add,)
                    @> listToAdditiveChain
            x -> x
        )
        False

-- | Multiplies like terms e.g. (2x * 3x) -> 6x^2
multiplyLikeTerms :: Simplifier
multiplyLikeTerms =
    let termReducer :: (MultiplicativeOperator, Value) -> DM.Map Value [(MultiplicativeOperator, Value)] -> DM.Map Value [(MultiplicativeOperator, Value)]
        termReducer (op, val) map =
            let (trueVal, trueExp) =
                    case val of
                        ExpChain a b -> (a, b)
                        x -> (x, Scalar 1)
             in adjustWithDefault
                    (\c -> (op, trueExp) : c)
                    []
                    trueVal
                    map
        termMapElemToValue :: (Value, [(MultiplicativeOperator, Value)]) -> Value
        termMapElemToValue (val, xs) =
            let mapListLink (op, val) =
                    let newOp =
                            case op of
                                Mul -> Add
                                Div -> Sub
                     in (newOp, val)
                exponent =
                    xs
                        |@>| mapListLink
                        @> listToAdditiveChain
             in ExpChain val exponent
     in Simplifier
            "multiplyLikeTerms"
            ( \case
                MultiplicativeChain xs ->
                    LAIL.toList xs
                        @> foldr termReducer DM.empty
                        @> DM.assocs
                        |@>| ((Mul,) . termMapElemToValue)
                        @> listToMultiplicativeChain
                x -> x
            )
            False

simplifiers :: [Simplifier]
simplifiers =
    [ reduceSingleElementChain
    , sumLikeTerms
    , multiplyLikeTerms
    , simplifyMultiplyBy1
    , simplifyExpBy1
    , simplifyExp1ByAnything
    , performScalarExponentiation
    , simplifyDoubleNegative
    , simplifyAdd0
    , simplifyAdditiveTimesNeg1
    ]

convergentSimplifiers :: [Simplifier]
convergentSimplifiers = filter convergent simplifiers

-- | Simplifies a value
simplify :: Value -> Value
simplify = aggregateSimplifier simplifiers @> runSimplifier
