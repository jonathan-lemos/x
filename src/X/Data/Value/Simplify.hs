{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module X.Data.Value.Simplify where

import Data.Foldable
import qualified Data.Map as DM
import Data.Number.CReal
import X.Data.Operator
import X.Data.Value
import X.Data.Value.Simplifier
import X.Utils.CReal
import X.Utils.LeftToRight
import X.Utils.List
import X.Utils.Map

-- | Converts an additive/multiplicative chain of a single element into that element.
simplifySingleElementChain :: Simplifier
simplifySingleElementChain =
    mkSimplifier "single element chain" $ \case
        AdditiveChain x [] -> x
        MultiplicativeChain x [] -> x
        x -> x

{- | Converts x^0 to 1, x^1 to x

 0^0 will be simplified to 1. This will be changed when the evaluation engine can report errors.
-}
simplifyExponentiationBy0Or1 :: Simplifier
simplifyExponentiationBy0Or1 =
    mkSimplifier "x^0 and x^1" $ \case
        ExpChain b (Scalar 1) -> b
        ExpChain _ (Scalar 0) -> Scalar 1
        x -> x

-- | Converts 0x to 0
simplifyMultiplyBy0 :: Simplifier
simplifyMultiplyBy0 =
    mkSimplifier "0x" $
        transformMultiplicativeChain
            ( \ms ->
                if any (snd |@>| (== Scalar 0)) ms
                    then Scalar 0
                    else multiplicativeChainFromList ms
            )

-- | Converts x+0 to x
simplifyAdd0 :: Simplifier
simplifyAdd0 = mkSimplifier "x+0" $ mapAdditiveChain (filter $ snd |@>| (/= Scalar 0))

-- | Converts 1x to x
simplifyMultiply1 :: Simplifier
simplifyMultiply1 = mkSimplifier "1x" $ mapMultiplicativeChain (filter $ snd |@>| (/= Scalar 1))

-- | Converts + (-x) to - x and - (-x) to + x
simplifyAddingNegative :: Simplifier
simplifyAddingNegative =
    mkSimplifier "+(-1) and -(1)" $
        let mapAdditiveTerm t =
                case t of
                    (Add, Scalar x) | x < 0 -> (Sub, Scalar (-x))
                    (Sub, Scalar x) | x < 0 -> (Add, Scalar (-x))
                    (x, y) -> (x, y)
         in mapAdditiveChain (fmap mapAdditiveTerm)

_partitionChainTerms :: [(o, Value)] -> ([(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)])
_partitionChainTerms =
    foldr
        ( \(o, v) (scalars, variables, additiveChains, multiplicativeChains, expChains) ->
            case v of
                Scalar _ -> ((o, v) : scalars, variables, additiveChains, multiplicativeChains, expChains)
                Variable _ -> (scalars, (o, v) : variables, additiveChains, multiplicativeChains, expChains)
                AdditiveChain _ _ -> (scalars, variables, (o, v) : additiveChains, multiplicativeChains, expChains)
                MultiplicativeChain _ _ -> (scalars, variables, additiveChains, (o, v) : multiplicativeChains, expChains)
                ExpChain _ _ -> (scalars, variables, additiveChains, multiplicativeChains, (o, v) : expChains)
        )
        ([], [], [], [], [])

_mapPartitionOutput :: ([(o, Value)] -> [(o, Value)]) -> ([(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)]) -> ([(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)])
_mapPartitionOutput fn (a, b, c, d, e) = (fn a, fn b, fn c, fn d, fn e)

-- | Sorts terms of a chain
simplifySortChainTerms :: Simplifier
simplifySortChainTerms =
    mkSimplifier "sort chain terms" $
        let sortGroup :: Ord o => [(o, Value)] -> [(o, Value)]
            sortGroup = sortByKey (\(_, l) -> (length (show l), show l))
         in mapAdditiveChain
                ( \ms ->
                    let (scalars, variables, additiveChains, multiplicativeChains, expChains) = _partitionChainTerms ms @> _mapPartitionOutput sortGroup
                     in variables <> expChains <> multiplicativeChains <> additiveChains <> scalars
                )
                |@>| mapMultiplicativeChain
                    ( \ms ->
                        let (scalars, variables, additiveChains, multiplicativeChains, expChains) = _partitionChainTerms ms @> _mapPartitionOutput sortGroup
                         in (scalars <> variables <> expChains <> multiplicativeChains <> additiveChains)
                    )

_valueToCoefficientAndMultiplier :: Value -> (CReal, Value)
_valueToCoefficientAndMultiplier v =
    case v of
        Scalar sc -> (sc, Scalar 1)
        MultiplicativeChain (Scalar sc) (x : xs) -> (sc, multiplicativeChainFromList (x : xs))
        _ -> (1, v)

_groupLikeChainTerms :: [(o, Value)] -> [[(CReal, (o, Value))]]
_groupLikeChainTerms ms =
    let groupKey (_op, val) = val @> _valueToCoefficientAndMultiplier @> snd
        groupVal (op, val) = (val @> _valueToCoefficientAndMultiplier @> fst, (op, val @> _valueToCoefficientAndMultiplier @> snd))
        terms = groupMap groupKey groupVal ms @> DM.toAscList
     in terms |@>| snd

_condenseAdditiveTerms :: [(AdditiveOperator, Value)] -> [(CReal, (AdditiveOperator, Value))]
_condenseAdditiveTerms as =
    let sumGroup =
            foldl'
                ( \totalCount (count, (op, _value)) ->
                    case op of
                        Add -> totalCount + count
                        Sub -> totalCount - count
                )
                0
        groupedTerms = _groupLikeChainTerms as
     in (groupedTerms |@>| \g -> (sumGroup g, (Add, head g @> snd @> snd)))

_condenseMultiplicativeTerms :: [(MultiplicativeOperator, Value)] -> [(CReal, (MultiplicativeOperator, Value))]
_condenseMultiplicativeTerms ms =
    let productGroup =
            foldl'
                ( \totalProduct (count, (op, _value)) ->
                    case op of
                        Mul -> totalProduct * count
                        Div -> totalProduct / count
                )
                1
        groupedTerms = _groupLikeChainTerms ms
     in (groupedTerms |@>| \g -> (productGroup g, (Mul, ExpChain (head g @> snd @> snd) (length ms @> fromIntegral @> Scalar))))

-- | Combines like additive terms e.g. x + 2x + 2 + 3 -> 3x + 5
simplifyLikeAdditiveTerms :: Simplifier
simplifyLikeAdditiveTerms = mkSimplifier "add like terms" $ mapAdditiveChain $ \as ->
    _condenseAdditiveTerms as
        |@>| \(scalar, (op, val)) ->
            (Add, MultiplicativeChain (Scalar scalar) [(Mul, additiveChainFromList [(op, val)])])

-- | Combines like multiplicative terms e.g. x * 3x * 2 * 5 -> 15x
simplifyLikeMultiplicativeTerms :: Simplifier
simplifyLikeMultiplicativeTerms = mkSimplifier "multiply like terms" $ mapMultiplicativeChain $ \ms ->
    _condenseMultiplicativeTerms ms
        |@>| \(scalar, (op, val)) ->
            (Mul, MultiplicativeChain (Scalar scalar) [(Mul, multiplicativeChainFromList [(op, val)])])

_condenseExpTerms :: [(MultiplicativeOperator, Value)] -> [(MultiplicativeOperator, Value)]
_condenseExpTerms =
    let groupKv (op, val) =
            case (op, val) of
                (Mul, ExpChain b e) -> (b, (Add, e))
                (Div, ExpChain b e) -> (b, (Sub, e))
                (Mul, v) -> (v, (Add, Scalar 1))
                (Div, v) -> (v, (Sub, Scalar 1))
        groups = groupMap (groupKv |@>| fst) (groupKv |@>| snd) |@>| DM.toAscList
        mapGroup (b, es) = ExpChain b (additiveChainFromList es)
     in groups ||@>|| mapGroup ||@>|| (Mul,)

-- | Combines exponential terms with the same base, e.g. x*x^2*x^y -> x^(3+y)
simplifyLikeExpTerms :: Simplifier
simplifyLikeExpTerms = mkSimplifier "combine like exponential terms" $ mapMultiplicativeChain _condenseExpTerms

-- | Evaluates x^y expressions when x and y are both scalars.
simplifyEvaluateScalarExp :: Simplifier
simplifyEvaluateScalarExp =
    mkSimplifier "evaluate a^b" $ \case
        ExpChain (Scalar x) (Scalar y) -> Scalar (x `safeExp` y)
        x -> x

simplifiers :: [Simplifier]
simplifiers =
    [ simplifySingleElementChain
    , simplifyExponentiationBy0Or1
    , simplifyMultiplyBy0
    , simplifyAdd0
    , simplifyMultiply1
    , simplifySortChainTerms
    , simplifyLikeAdditiveTerms
    , simplifyLikeMultiplicativeTerms
    , simplifyLikeExpTerms
    , simplifyEvaluateScalarExp
    ]

-- | Simplifies a value
simplify :: Value -> Value
simplify = aggregateSimplifier "simplify" simplifiers @> runSimplifier
