{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module X.Data.Value.Simplifier where

import Data.List
import X.Data.Value
import X.Utils.Function
import X.Utils.LeftToRight

{- | A simplifier turns a value into an identical, but less complicated version of itself
The simplifying function will be run until the value doesn't change.
-}
data Simplifier = Simplifier
    { simplifierName :: String
    -- ^ A string description of what the simplifier does. Used for simplifier equality.
    , simplifyingFunction :: Value -> Value
    -- ^ A function that converts a Value into a simplified version of that Value.
    , convergent :: Bool
    -- ^ True if the function, when called repeatedly, eventually converges to a value after which it no longer changes.
    }

mapSimplifierName :: (String -> String) -> Simplifier -> Simplifier
mapSimplifierName f s = s{simplifierName = f $ simplifierName s}

mapSimplifyingFunction :: ((Value -> Value) -> Value -> Value) -> Simplifier -> Simplifier
mapSimplifyingFunction f s = s{simplifyingFunction = f $ simplifyingFunction s}

mapConvergent :: (Bool -> Bool) -> Simplifier -> Simplifier
mapConvergent f s = s{convergent = f $ convergent s}

instance Eq Simplifier where
    (Simplifier aTitle _ aConv) == (Simplifier bTitle _ bConv) = (aTitle, aConv) == (bTitle, bConv)

instance Ord Simplifier where
    compare (Simplifier aTitle _ aConv) (Simplifier bTitle _ bConv) = compare (aTitle, aConv) (bTitle, bConv)

instance Show Simplifier where
    show = simplifierName |@>| show

-- | Applies the given simplifying function to all children of the given Value, then to the given Value itself
deepSimplify :: Simplifier -> Simplifier
deepSimplify =
    let deepFunc f v =
            let df = deepFunc f
             in f $ case v of
                    AdditiveChain xs -> AdditiveChain (xs |@>| df)
                    MultiplicativeChain xs -> MultiplicativeChain (xs |@>| df)
                    ExpChain b e -> ExpChain (df b) (df e)
                    Scalar sc -> Scalar sc
                    Variable v -> Variable v
     in mapSimplifyingFunction deepFunc

{- | Returns a function that applies the given simplifier deep
 If the simplifier is convergent, also runs the function repeatedly until the value doesn't change.
-}
runSimplifier :: Simplifier -> Value -> Value
runSimplifier simp =
    let modifiedSimplifier =
            if convergent simp
                then deepSimplify simp @> mapSimplifyingFunction fixedPoint
                else deepSimplify simp
     in simplifyingFunction modifiedSimplifier

aggregateSimplifier :: [Simplifier] -> Simplifier
aggregateSimplifier xs =
    let (convergentSimplifiers, nonconvergentSimplifiers) = partition convergent xs
        aggregateConvergent =
            Simplifier
                "convergent simplifiers"
                (convergentSimplifiers |@>| deepSimplify |@>| simplifyingFunction |@>| fixedPoint @> foldr (.) id)
                True
     in case nonconvergentSimplifiers of
            [] -> aggregateConvergent
            xs ->
                Simplifier
                    "aggregate simplifier"
                    ( xs
                        |@>| deepSimplify
                        |@>| simplifyingFunction
                        |@>| (runSimplifier aggregateConvergent .)
                        @> foldr (.) id
                    )
                    False
