{-# LANGUAGE LambdaCase #-}

module X.Data.Value.Simplify where

import qualified X.Data.LeftAssociativeInfixChain as LAIL
import X.Data.Value
import X.Data.Value.Simplifier
import X.Utils.LeftToRight

-- | Converts an additive/multiplicative chain of a single element into that element.
simplifySingleElementChain :: Simplifier
simplifySingleElementChain =
    mkSimplifier "single element chain" $ \case
        AdditiveChain (LAIL.Leaf x) -> x
        MultiplicativeChain (LAIL.Leaf x) -> x
        x -> x

simplifiers :: [Simplifier]
simplifiers =
    [ simplifySingleElementChain
    ]

-- | Simplifies a value
simplify :: Value -> Value
simplify = aggregateSimplifier "simplify" simplifiers @> runSimplifier
