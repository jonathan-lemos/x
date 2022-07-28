module X.Data.Value.Evaluate where

import X.Data.Value
import X.Data.Context
import X.Data.Operator
import Data.Maybe
import qualified Data.Map as DM
import Data.IntMap.Merge.Lazy (SimpleWhenMatched)
import X.Data.Value.Simplify (Simplifier)

isScalar :: Value -> Bool
isScalar (Scalar _) = True
isScalar _ = False

-- | Converts anything times 0 to 0
_simplifyMultiplyBy0 :: Value -> Value
_simplifyMultiplyBy0 v =
    case v of
        MultiplicativeChain (Scalar 0) _ -> Scalar 0
        MultiplicativeChain _ xs | any (snd |@>| (== Scalar 0)) xs -> Scalar 0
        v -> v

-- | Converts -(-x) to +x, +(-x) to -x, *(1/x) to /x, /(1/x) to *x
_simplifyDoubleNegatives :: Value -> Value
_simplifyDoubleNegatives v =
    let mapAdditiveTerm t =
            case t of
                (Add, x) | x < 0 -> (Sub, -x)
                (Sub, x) | x < 0 -> (Add, -x)
                (x, y) -> (x, y)

        mapMultiplicativeTerm t =
            case t of
                (Mul, x) | abs x > 0 && abs x < 1 -> (Div, 1 / x)
                (Div, x) | abs x > 0 && abs x < 1 -> (Mul, 1 / x)
                (x, y) -> (x, y)

    in case v of
        AdditiveChain v vs -> AdditiveChain v $ vs |@>| mapAdditiveTerm
        MultiplicativeChain v vs -> AdditiveChain v $ vs |@>| mapMultiplicativeTerm
        v -> v

-- | Removes +- 0's from additive chains and */ 1's from multiplicative chains.
-- If the expression reduces to a single term, lifts it out of the (now redundant) chain layer.
_simplifyChainIdentity :: Value -> Value
_simplifyChainIdentity v =
    let stripIdentities identity c v vs =
            let newvs = vs
                        |@>| _simplifyChainIdentity
                        @> filter (!= identity)
            in case (v, vs) of
                (x, []) -> x
                (x, y:ys) | x == identity -> c y ys
                (x, y) -> c x y
    in case v of
        AdditiveChain v vs -> stripIdentities (Scalar 0) AdditiveChain v vs
        MultiplicativeChain v vs -> stripIdentities (Scalar 1) MultiplicativeChain v vs
        v -> v

-- | Sums up the scalar terms of an additive chain. Multiplies up the scalar terms of a multiplicative chain.
-- If the expression reduces to a single term, lifts it out of the (now redundant) chain layer.
_groupChainScalars :: Value -> Value
_groupChainScalars v =
    case v of
        AdditiveChain v vs ->
            foldr
            (\(op, v) (s, l) -> case (op, v) of
                (Add, Scalar sc) -> (s + sc, l)
                (Sub, Scalar sc) -> (s - sc, l)
                (_, val) -> (s, val : l))
            (case v of
                Scalar sc -> (sc, [])
                v -> (0, [v]))
            vs
        MultiplicativeChain v vs ->
            foldr
            (\(op, v) (s, l) -> case (op, v) of
                (Mul, Scalar sc) -> (s * sc, l)
                (Div, Scalar sc) -> (s / sc, l)
                (_, val) -> (s, val : l))
            (case v of
                Scalar sc -> (sc, [])
                v -> (0, [v]))
            vs
        v -> v

_simplifyValue :: Simplifier
_simplifyValue = _simplifyChainIdentity

_valueWithoutCoefficient :: Value -> Maybe Value
_valueWithoutCoefficient v =
    case _simplifyValue v of
        Scalar _ -> Nothing
        Variable v -> Variable v


_groupLikeTerms :: [(Operator, Value)] -> [(Value, (Operator, Maybe Value))]
_groupLikeTerms xs = undefined

evaluateValue :: Value -> Context -> Value
evaluateValue (Scalar n) _ctx = Scalar n
evaluateValue (Variable s) ctx = fromMaybe (Variable s) (get s ctx)
evaluateValue (AdditiveChain x xs) ctx = undefined

