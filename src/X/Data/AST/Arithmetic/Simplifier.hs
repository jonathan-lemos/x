module X.Data.AST.Arithmetic.Simplifier (Simplifier (runSimplifier, simplifierName), deepSimplify, mkSimplifier, aggregateSimplifier, originalFunction) where

import X.Data.AST.Arithmetic
import X.Data.AST.ArithmeticUnion
import X.Data.LeftAssociativeInfixList
import X.Utils.Function
import X.Utils.LeftToRight

{- | A simplifier turns a value into an identical, but less complicated version of itself
The simplifying function will be run until the value doesn't change.
-}
data Simplifier = Simplifier {simplifierName :: String, runSimplifier :: ArithmeticUnion -> ArithmeticUnion, originalFunction :: ArithmeticUnion -> ArithmeticUnion}

instance Eq Simplifier where
    (Simplifier aTitle _ _) == (Simplifier bTitle _ _) = aTitle == bTitle

instance Ord Simplifier where
    compare (Simplifier aTitle _ _) (Simplifier bTitle _ _) = compare aTitle bTitle

instance Show Simplifier where
    show = simplifierName |@>| show

-- | Applies the given simplifying function to all children of the given ArithmeticUnion, then to the given ArithmeticUnion itself
deepSimplify :: (ArithmeticUnion -> ArithmeticUnion) -> (ArithmeticUnion -> ArithmeticUnion)
deepSimplify f v =
    let ds = deepSimplify f
     in f $ case v of
            AUAdditiveChain (AdditiveChain x) -> AUAddtitiveChain (AdditiveChain (x |@>| ds))
            AUAdditiveChain (AdditiveChain (tree :<: (op, x))) ->
                case ds (AUAdditiveChain (AdditiveChain tree)) of
                    (AUAdditiveChain (AdditiveChain subtree)) -> AUAdditiveChain (AdditiveChain (subtree :<: (op, ds x)))
                    x -> x
            AUAdditiveChain (AdditiveChain x) -> AUAdditiveChain (AdditiveChain (ds x))
            AUMultiplicativeChain (MultiplicativeChain (tree :<: (op, x))) ->
                case ds (AUMultiplicativeChain (MultiplicativeChain tree)) of
                    MultiplicativeChain subtree -> AUMultiplicativeChain (MultiplicativeChain (subtree :<: (op, ds x)))
                    x -> x
            AUMultiplicativeChain (MultiplicativeChain x) -> AUMultiplicativeChain (MultiplicativeChain (ds x))
            AUExpChain (Exponentiation xs) -> AUExpChain (Exponentiation (xs |@>| ds))
            Scalar sc -> Scalar sc
            Variable v -> Variable v

mkSimplifier :: String -> (ArithmeticUnion -> ArithmeticUnion) -> Simplifier
mkSimplifier title f =
    Simplifier title (deepSimplify f @> fixedPoint) f

aggregateSimplifier :: String -> [Simplifier] -> Simplifier
aggregateSimplifier title xs =
    mkSimplifier title $
        xs |@>| runSimplifier @> foldl1 (|@>|)
