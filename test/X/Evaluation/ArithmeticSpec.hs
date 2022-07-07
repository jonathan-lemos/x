module  X.Evaluation.ArithmeticSpec where

import X.Evaluation.Arithmetic
import X.Evaluation.ToValue
import X.Data.State.Value
import X.Data.State.XState
import Test.Hspec
import X.TestUtils.Should
import X.TestUtils.State
import X.Data.Unit.Arithmetic
import X.Data.Unit.Unit
import Harness.TestCase
import Harness.With
import X.TestUtils.Unit
import X.Control.Try

spec :: Spec
spec = parallel $ do
    let state = mkState [("a", Numeric 4 Nothing), ("b", Numeric 9 (Just $ BaseUnit "kg"))]

    let gus = (`getUnit` state)

    let withState :: (Value -> Value -> XState -> Try Value) -> Value -> Value -> Try Value
        withState f a b = f a b state

    let add = withState addValues
    let sub = withState subValues
    let mul = withState multValues
    let div = withState divValues
    let exp a b = expValues a b state

    let anonymousNewton = (gus "kg" `unitMaybeMult` gus "m") `unitMaybeDiv` (gus "s" `unitMaybeExpScalar` 2)

    let a = Numeric 2 Nothing
    let b = Numeric 3 Nothing
    let c = Numeric 5 (gus "N")
    let d = Numeric 7 anonymousNewton

    desc "addValues" $ do
        a `add` b `shouldEq` Success (Numeric 5 Nothing) `withTitle` "adding scalars"
        b `add` a `shouldEq` Success (Numeric 5 Nothing) `withTitle` "adding scalars is commutative"
        d `add` d `shouldEq` Success (Numeric 14 anonymousNewton) `withTitle` "adding anonymous newton"
        c `add` d `shouldEq` Success (Numeric 12 (Just $ toUnit "N")) `withTitle` "adding newton to anonymous newton"
        d `add` c `shouldEq` Success (Numeric 12 anonymousNewton) `withTitle` "adding anonymous newton to newton"
        a `add` c `shouldEq` Failure "Cannot add unitless quantity and N" `withTitle` "adding unitless and N"
        c `add` a `shouldEq` Failure "Cannot add N and unitless quantity" `withTitle` "adding N and unitless"

    desc "subValues" $ do
        b `sub` a `shouldEq` Success (Numeric 1 Nothing) `withTitle` "subtracting scalars"
        d `sub` d `shouldEq` Success (Numeric 0 anonymousNewton) `withTitle` "subtracting anonymous newtons"
        c `sub` d `shouldEq` Success (Numeric (-2) (Just $ toUnit "N")) `withTitle` "subtracting anonymous newton from newton"
        d `sub` c `shouldEq` Success (Numeric 2 anonymousNewton) `withTitle` "subtracting newton from anonymous newton"
        a `sub` c `shouldEq` Failure "Cannot subtract unitless quantity and N" `withTitle` "subtracting unitless and N"
        c `sub` a `shouldEq` Failure "Cannot subtract N and unitless quantity" `withTitle` "subtracting N and unitless"

    desc "multValues" $ do
        a `mul` b `shouldEq` Success (Numeric 6 Nothing) `withTitle` "multiplying scalar"
        b `mul` a `shouldEq` Success (Numeric 6 Nothing) `withTitle` "multiplying scalar is commutative"
        a `mul` c `shouldEq` Success (Numeric 10 (gus "N")) `withTitle` "multiplying scalar and newton"
        c `mul` a `shouldEq` Success (Numeric 10 (gus "N")) `withTitle` "multiplying scalar and newton is commutative"
        c `mul` c `shouldEq` Success (Numeric 25 (gus "N" `unitMaybeMult` gus "N")) `withTitle` "squaring newton -- todo: should be N^2"
        c `mul` d `shouldEq` Success (Numeric 35 (gus "N" `unitMaybeMult` anonymousNewton)) `withTitle` "multiplying newton and anonymous newton -- todo: should be N^2"
        d `mul` c `shouldEq` Success (Numeric 35 (anonymousNewton `unitMaybeMult` gus "N")) `withTitle` "multiplying anonymous newton and newton -- todo: should be kg*m/s^2"

    desc "divValues" $ do
        a `div` b `shouldEq` Success (Numeric (2 / 3) Nothing) `withTitle` "dividing 2/3"
        b `div` a `shouldEq` Success (Numeric (3 / 2) Nothing) `withTitle` "dividing 3/2"
        a `div` c `shouldEq` Success (Numeric (2 / 5) (unitMaybeReciprocal $ gus "N")) `withTitle` "2 / 5N"
        c `div` a `shouldEq` Success (Numeric (5 / 2) (gus "N")) `withTitle` "5N / 2"
        c `div` c `shouldEq` Success (Numeric 1 (gus "N" `unitMaybeDiv` gus "N")) `withTitle` "5N / 5N -- todo: should be unitless"
        c `div` d `shouldEq` Success (Numeric (5 / 7) (gus "N" `unitMaybeDiv` anonymousNewton)) `withTitle` "5N / 7kg*m/s^2 -- todo: should be unitless"
        d `div` c `shouldEq` Success (Numeric (7 / 5) (anonymousNewton `unitMaybeDiv` gus "N")) `withTitle` "7kg*m/s^2 / 5N -- todo: should be kg*m/s^2"

    desc "expValues" $ do
        a `exp` b `shouldEq` Success (Numeric 8 Nothing) `withTitle` "2^3"
        b `exp` a `shouldEq` Success (Numeric 9 Nothing) `withTitle` "3^2"
        c `exp` a `shouldEq` Success (Numeric 25 (gus "N" `unitMaybeExpScalar` 2)) `withTitle` "5N^2"
        d `exp` b `shouldEq` Success (Numeric 343 (anonymousNewton `unitMaybeExpScalar` 3)) `withTitle` "(7kg*m/s^2)^3"
        a `exp` c `shouldEq` Failure "Cannot exponentiate by a unit quantity (N). Must exponentiate by a unitless quantity." `withTitle` "2^(5N)"
        b `exp` d `shouldEq` Failure "Cannot exponentiate by a unit quantity (((kg*m)/(s^2.0))). Must exponentiate by a unitless quantity." `withTitle` "3^(7N) -- todo unparenthesize error message"