module X.Data.Value.Simplify where

import Data.Bifunctor
import Data.Number.CReal
import X.Data.Operator
import X.Data.Value
import X.Utils.Function
import X.Utils.LeftToRight
import X.Utils.List

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
_simplifyMultiplyingReciprocal :: Value -> Value
_simplifyMultiplyingReciprocal =
    let mapMultiplicativeTerm t =
            case t of
                (Mul, Scalar x) | abs x > 0 && abs x < 1 -> (Div, Scalar (1 / x))
                (Div, Scalar x) | abs x > 0 && abs x < 1 -> (Mul, Scalar (1 / x))
                (x, y) -> (x, y)
     in mapMultiplicativeChain (fmap mapMultiplicativeTerm)

{- | Removes +- 0's from additive chains and */ 1's from multiplicative chains.
 If the expression reduces to a single term, lifts it out of the (now redundant) chain layer.
-}
_simplifyChainIdentity :: Value -> Value
_simplifyChainIdentity =
    let stripIdentities identity =
            fmap (second _simplifyChainIdentity)
                |@>| filter (snd |@>| (/= identity))
     in mapAdditiveChain (stripIdentities (Scalar 0))
            |@>| mapMultiplicativeChain (stripIdentities (Scalar 1))

{- | Sums up the scalar terms of an additive chain. Multiplies up the scalar terms of a multiplicative chain.
 If the expression reduces to a single term, lifts it out of the (now redundant) chain layer.
-}
_simplifyGroupChainScalars :: Value -> Value
_simplifyGroupChainScalars =
    mapAdditiveChain
        ( \ms ->
            foldr
                ( \(op, v) (s, l) -> case (op, v) of
                    (Add, Scalar sc) -> (s + sc, l)
                    (Sub, Scalar sc) -> (s - sc, l)
                    (op, val) -> (s, (op, val) : l)
                )
                (0, [])
                ms
                @> \(sc, rest) -> rest <> [(Add, Scalar sc)]
        )
        |@>| mapMultiplicativeChain
            ( \ms ->
                foldr
                    ( \(op, v) (s, l) -> case (op, v) of
                        (Mul, Scalar sc) -> (s * sc, l)
                        (Div, Scalar sc) -> (s / sc, l)
                        (op, val) -> (s, (op, val) : l)
                    )
                    (1, [])
                    ms
                    @> \(sc, rest) -> (Mul, Scalar sc) : rest
            )

_partitionChainTerms :: [(o, Value)] -> ([(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)])
_partitionChainTerms =
    foldr
        ( \(o, v) (scalars, variables, additiveChains, multiplicativeChains, expChains, negates) ->
            case v of
                Scalar _ -> ((o, v) : scalars, variables, additiveChains, multiplicativeChains, expChains, negates)
                Variable _ -> (scalars, (o, v) : variables, additiveChains, multiplicativeChains, expChains, negates)
                AdditiveChain _ _ -> (scalars, variables, (o, v) : additiveChains, multiplicativeChains, expChains, negates)
                MultiplicativeChain _ _ -> (scalars, variables, additiveChains, (o, v) : multiplicativeChains, expChains, negates)
                ExpChain _ _ -> (scalars, variables, additiveChains, multiplicativeChains, (o, v) : expChains, negates)
                Negate _ -> (scalars, variables, additiveChains, multiplicativeChains, expChains, (o, v) : negates)
        )
        ([], [], [], [], [], [])

_mapPartitionOutput :: ([(o, Value)] -> [(o, Value)]) -> ([(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)]) -> ([(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)], [(o, Value)])
_mapPartitionOutput fn (a, b, c, d, e, f) = (fn a, fn b, fn c, fn d, fn e, fn f)

_simplifySortChainTerms :: Value -> Value
_simplifySortChainTerms =
    let sortGroup :: Ord o => [(o, Value)] -> [(o, Value)]
        sortGroup = sortByKey (\(_, l) -> (length (show l), show l))
     in mapAdditiveChain
            ( \ms ->
                let (scalars, variables, additiveChains, multiplicativeChains, expChains, negates) = _partitionChainTerms ms @> _mapPartitionOutput sortGroup
                 in variables <> expChains <> multiplicativeChains <> additiveChains <> negates <> scalars
            )
            |@>| mapMultiplicativeChain
                ( \ms ->
                    let (scalars, variables, additiveChains, multiplicativeChains, expChains, negates) = _partitionChainTerms ms @> _mapPartitionOutput sortGroup
                     in (scalars <> variables <> expChains <> multiplicativeChains <> additiveChains <> negates)
                )

_valueToCoefficientAndMultiplier :: Value -> (CReal, Maybe Value)
_valueToCoefficientAndMultiplier v =
    case v of
        Scalar _ -> (sc, Nothing)
        Variable _ -> (1, Just v)

_simplifyLikeTerms :: Value -> Value
_simplifyLikeTerms = undefined

_deepSimplify :: (Value -> Value) -> Value -> Value
_deepSimplify f v =
    let ds = _deepSimplify f
     in f $ case v of
            AdditiveChain x xs -> AdditiveChain (ds x) (xs |@>| second ds)
            MultiplicativeChain x xs -> MultiplicativeChain (ds x) (xs |@>| second ds)
            ExpChain b e -> ExpChain (ds b) (ds e)
            Scalar sc -> Scalar sc
            Variable v -> Variable v
            Negate x -> Negate (ds x)

simplify :: Value -> Value
simplify =
    let simplifyPass =
            _simplifyMultiplyBy0
                |@>| _simplifyDoubleNegatives
                |@>| _simplifyChainIdentity
                |@>| _simplifyGroupChainScalars
                |@>| _simplifySortChainTerms
     in fixedPoint (_deepSimplify simplifyPass)
