{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module X.Data.Value.Simplifier (Simplifier (runSimplifier, simplifierName), deepSimplify, mkSimplifier, aggregateSimplifier) where

import Data.Bifunctor
import X.Data.Value
import X.Utils.Function
import X.Utils.LeftToRight

{- | A simplifier turns a value into an identical, but less complicated version of itself
The simplifying function will be run until the value doesn't change.
-}
data Simplifier = Simplifier {simplifierName :: String, runSimplifier :: Value -> Value, originalFunction :: Value -> Value}

instance Eq Simplifier where
    (Simplifier aTitle _ _) == (Simplifier bTitle _ _) = aTitle == bTitle

instance Ord Simplifier where
    compare (Simplifier aTitle _ _) (Simplifier bTitle _ _) = compare aTitle bTitle

instance Show Simplifier where
    show = simplifierName |@>| show

-- | Applies the given simplifying function to all children of the given Value, then to the given Value itself
deepSimplify :: (Value -> Value) -> (Value -> Value)
deepSimplify f v =
    let ds = deepSimplify f
     in f $ case v of
            AdditiveChain x xs -> AdditiveChain (ds x) (xs |@>| second ds)
            MultiplicativeChain x xs -> MultiplicativeChain (ds x) (xs |@>| second ds)
            ExpChain b e -> ExpChain (ds b) (ds e)
            Scalar sc -> Scalar sc
            Variable v -> Variable v

mkSimplifier :: String -> (Value -> Value) -> Simplifier
mkSimplifier title f =
    Simplifier title (deepSimplify f @> fixedPoint) f

aggregateSimplifier :: String -> [Simplifier] -> Simplifier
aggregateSimplifier title xs =
    mkSimplifier title $
        xs |@>| runSimplifier @> foldl1 (|@>|)
