{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module TestUtils.DSL.Value where

import qualified X.Data.LeftAssociativeInfixChain as LAIC
import X.Data.Value
import X.Data.Operator
import Data.Number.CReal

class DSLValueLike a where
    dslToValue :: a -> Value

instance DSLValueLike CReal where
    dslToValue = Scalar

instance DSLValueLike String where
    dslToValue = Variable

instance DSLValueLike Value where
    dslToValue = id

testInfixAdditiveOperation :: forall a b. (DSLValueLike a, DSLValueLike b) => AdditiveOperator -> a -> b -> Value
testInfixAdditiveOperation op a b =
    case (dslToValue a, dslToValue b) of
        (AdditiveChain as, AdditiveChain bs) -> AdditiveChain $ LAIC.concat as op bs
        (AdditiveChain as, x) -> AdditiveChain $ LAIC.append as op x
        (x, AdditiveChain as) -> AdditiveChain $ LAIC.prepend x op as
        (x, y) -> AdditiveChain $ LAIC.Link (LAIC.Leaf x) op y

(@+) :: (DSLValueLike a, DSLValueLike b) => a -> b -> Value
(@+) = testInfixAdditiveOperation Add

infixl 6 @+

(@-) :: forall a b. (DSLValueLike a, DSLValueLike b) => a -> b -> Value
(@-) = testInfixAdditiveOperation Sub

infixl 6 @-

testInfixMultiplicativeOperation :: forall a b. (DSLValueLike a, DSLValueLike b) => MultiplicativeOperator -> a -> b -> Value
testInfixMultiplicativeOperation op a b =
    case (dslToValue a, dslToValue b) of
        (MultiplicativeChain as, MultiplicativeChain bs) -> MultiplicativeChain $ LAIC.concat as op bs
        (MultiplicativeChain as, x) -> MultiplicativeChain $ LAIC.append as op x
        (x, MultiplicativeChain as) -> MultiplicativeChain $ LAIC.prepend x op as
        (x, y) -> MultiplicativeChain $ LAIC.Link (LAIC.Leaf x) op y

(@*) :: forall a b. (DSLValueLike a, DSLValueLike b) => a -> b -> Value
(@*) = testInfixMultiplicativeOperation Mul

infixl 7 @*

(@/) :: forall a b. (DSLValueLike a, DSLValueLike b) => a -> b -> Value
(@/) = testInfixMultiplicativeOperation Div

infixl 7 @/

(@^) :: forall a b. (DSLValueLike a, DSLValueLike b) => a -> b -> Value
a @^ b = ExpChain (dslToValue a) (dslToValue b)

infixr 8 @^

ac :: (DSLValueLike v) => v -> Value
ac = AdditiveChain . LAIC.Leaf . dslToValue

mc :: (DSLValueLike v) => v -> Value
mc = MultiplicativeChain . LAIC.Leaf . dslToValue

sc :: CReal -> Value
sc = Scalar
