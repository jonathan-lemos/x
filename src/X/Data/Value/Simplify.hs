{-# LANGUAGE TupleSections #-}
module X.Data.Value.Simplify where

import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as DM
import Data.Number.CReal
import X.Data.Operator
import X.Data.Value
import X.Utils.Function
import X.Utils.LeftToRight
import X.Utils.List
import X.Utils.Map
import Data.List

{- | A simplifier turns a value into an identical, but less complicated version of itself
A simplifier should be idempotent, meaning f(f(x)) == f(x).
-}
type Simplifier = Value -> Value

-- | Converts an additive/multiplicative chain of a single element into that element.
_simplifySingleElementChain :: Simplifier
_simplifySingleElementChain v =
    case v of
        AdditiveChain x [] -> x
        MultiplicativeChain x [] -> x
        x -> x

{- | Converts x^0 to 1, x^1 to x

 0^0 will be simplified to 1. This will be changed when the evaluation engine can report errors.
-}
_simplifyExponentiationBy0Or1 :: Simplifier
_simplifyExponentiationBy0Or1 v =
    case v of
        ExpChain b (Scalar 1) -> b
        ExpChain _ (Scalar 0) -> Scalar 1
        x -> x

-- | Converts 0x to 0
_simplifyMultiplyBy0 :: Simplifier
_simplifyMultiplyBy0 =
    transformMultiplicativeChain
        ( \ms ->
            if any (snd |@>| (== Scalar 0)) ms
                then Scalar 0
                else multiplicativeChainFromList ms
        )

-- | Converts x+0 to x
_simplifyAdd0 :: Simplifier
_simplifyAdd0 = mapAdditiveChain (filter $ snd |@>| (/= Scalar 0))

-- | Converts 1x to x
_simplifyMultiply1 :: Simplifier
_simplifyMultiply1 = mapMultiplicativeChain (filter $ snd |@>| (/= Scalar 1))

-- | Converts + (-x) to - x and - (-x) to + x
_simplifyAddingNegative :: Simplifier
_simplifyAddingNegative =
    let mapAdditiveTerm t =
            case t of
                (Add, Scalar x) | x < 0 -> (Sub, Scalar (-x))
                (Sub, Scalar x) | x < 0 -> (Add, Scalar (-x))
                (x, y) -> (x, y)
     in mapAdditiveChain (fmap mapAdditiveTerm)

-- | Converts *(1/x) to /x, /(1/x) to *x
_simplifyMultiplyingByReciprocal :: Simplifier
_simplifyMultiplyingByReciprocal =
    let mapMultiplicativeTerm t =
            case t of
                (Mul, Scalar x) | abs x > 0 && abs x < 1 -> (Div, Scalar (1 / x))
                (Div, Scalar x) | abs x > 0 && abs x < 1 -> (Mul, Scalar (1 / x))
                (x, y) -> (x, y)
     in mapMultiplicativeChain (fmap mapMultiplicativeTerm)

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

_simplifySortChainTerms :: Simplifier
_simplifySortChainTerms =
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
        groupVal (op, val) = (val @> _valueToCoefficientAndMultiplier @> fst, (op, val))
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

_simplifyLikeAdditiveTerms :: Simplifier
_simplifyLikeAdditiveTerms = mapAdditiveChain $ \as ->
    _condenseAdditiveTerms as
        |@>| \(scalar, (op, val)) ->
            (Add, MultiplicativeChain (Scalar scalar) [(Mul, additiveChainFromList [(op, val)])])

-- TODO: support combining x^2 and x
_simplifyLikeMultiplicativeTerms :: Simplifier
_simplifyLikeMultiplicativeTerms = mapMultiplicativeChain $ \ms ->
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
_simplifyLikeExpTerms :: Simplifier
_simplifyLikeExpTerms = mapMultiplicativeChain _condenseExpTerms

_deepSimplify :: (Value -> Value) -> Value -> Value
_deepSimplify f v =
    let ds = _deepSimplify f
     in f $ case v of
            AdditiveChain x xs -> AdditiveChain (ds x) (xs |@>| second ds)
            MultiplicativeChain x xs -> MultiplicativeChain (ds x) (xs |@>| second ds)
            ExpChain b e -> ExpChain (ds b) (ds e)
            Scalar sc -> Scalar sc
            Variable v -> Variable v

simplifiers :: [Simplifier]
simplifiers = [
    _simplifySingleElementChain,
    _simplifyExponentiationBy0Or1,
    _simplifyMultiplyBy0,
    _simplifyAdd0,
    _simplifyMultiply1,
    _simplifyMultiplyingByReciprocal,
    _simplifySortChainTerms,
    _simplifyLikeAdditiveTerms,
    _simplifyLikeMultiplicativeTerms,
    _simplifyLikeExpTerms]

simplify :: Value -> Value
simplify =
    let simplifyPass = foldl1' (|@>|) simplifiers
     in fixedPoint (_deepSimplify simplifyPass)
