{-# LANGUAGE LambdaCase #-}
module X.Data.AST.Arithmetic.Union where

import X.Data.AST.Arithmetic
import X.Data.Display

data ArithmeticUnion
    = AUAdditiveChain AdditiveChain
    | AUMultiplicativeChain MultiplicativeChain
    | AUExpChain ExpChain
    | AUFactor Factor
    deriving (Eq, Ord, Show)

instance Display ArithmeticUnion where
    display = \case
        AUAdditiveChain c -> display c
        AUMultiplicativeChain c -> display c
        AUExpChain c -> display c
        AUFactor f -> display f

liftAU ::

