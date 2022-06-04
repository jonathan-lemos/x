module Evaluation.ArithmeticSpec where

import Evaluation.Arithmetic
import Evaluation.ToValue
import State.Value
import State.XState
import Test.Hspec
import TestUtils.Should
import TestUtils.State
import Unit.Arithmetic
import Unit.Unit

spec :: Spec
spec = parallel $ do
    let state = mkState [("a", Numeric 4 Nothing), ("b", Numeric 9 (Just $ BaseUnit "kg"))]

    let gus = (`getUnit` state)

    let withState :: (Value -> Value -> XState -> Either String Value) -> Value -> Value -> Either String Value
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

    shouldBeSpec
        "addValues"
        [ (a `add` b, Right $ Numeric 5 Nothing, "adding scalars")
        , (b `add` a, Right $ Numeric 5 Nothing, "adding scalars is commutative")
        , (d `add` d, Right $ Numeric 14 anonymousNewton, "adding anonymous newton")
        , (c `add` d, Right $ Numeric 12 (gus "N"), "adding newton to anonymous newton")
        , (d `add` c, Right $ Numeric 12 anonymousNewton, "adding anonymous newton to newton")
        , (a `add` c, Left "Cannot add unitless quantity and N", "adding unitless and N")
        , (c `add` a, Left "Cannot add N and unitless quantity", "adding N and unitless")
        ]

    shouldBeSpec
        "subValues"
        [ (b `sub` a, Right $ Numeric 1 Nothing, "subtracting scalars")
        , (d `sub` d, Right $ Numeric 0 anonymousNewton, "subtracting anonymous newtons")
        , (c `sub` d, Right $ Numeric (-2) (gus "N"), "subtracting anonymous newton from newton")
        , (d `sub` c, Right $ Numeric 2 anonymousNewton, "subtracting newton from anonymous newton")
        , (a `sub` c, Left "Cannot subtract unitless quantity and N", "subtracting unitless and N")
        , (c `sub` a, Left "Cannot subtract N and unitless quantity", "subtracting N and unitless")
        ]

    shouldBeSpec
        "multValues"
        [ (a `mul` b, Right $ Numeric 6 Nothing, "multiplying scalar")
        , (b `mul` a, Right $ Numeric 6 Nothing, "multiplying scalar is commutative")
        , (a `mul` c, Right $ Numeric 10 (gus "N"), "multiplying scalar and newton")
        , (c `mul` a, Right $ Numeric 10 (gus "N"), "multiplying scalar and newton is commutative")
        , (c `mul` c, Right $ Numeric 25 (gus "N" `unitMaybeMult` gus "N"), "squaring newton -- todo: should be N^2")
        , (c `mul` d, Right $ Numeric 35 (gus "N" `unitMaybeMult` anonymousNewton), "multiplying newton and anonymous newton -- todo: should be N^2")
        , (d `mul` c, Right $ Numeric 35 (anonymousNewton `unitMaybeMult` gus "N"), "multiplying anonymous newton and newton -- todo: should be kg*m/s^2")
        ]

    shouldBeSpec
        "divValues"
        [ (a `div` b, Right $ Numeric (2 / 3) Nothing, "dividing 2/3")
        , (b `div` a, Right $ Numeric (3 / 2) Nothing, "dividing 3/2")
        , (a `div` c, Right $ Numeric (2 / 5) (unitMaybeReciprocal $ gus "N"), "2 / 5N")
        , (c `div` a, Right $ Numeric (5 / 2) (gus "N"), "5N / 2")
        , (c `div` c, Right $ Numeric 1 (gus "N" `unitMaybeDiv` gus "N"), "5N / 5N -- todo: should be unitless")
        , (c `div` d, Right $ Numeric (5 / 7) (gus "N" `unitMaybeDiv` anonymousNewton), "5N / 7kg*m/s^2 -- todo: should be unitless")
        , (d `div` c, Right $ Numeric (7 / 5) (anonymousNewton `unitMaybeDiv` gus "N"), "7kg*m/s^2 / 5N -- todo: should be kg*m/s^2")
        ]

    shouldBeSpec
        "expValues"
        [ (a `exp` b, Right $ Numeric 8 Nothing, "2^3")
        , (b `exp` a, Right $ Numeric 9 Nothing, "3^2")
        , (c `exp` a, Right $ Numeric 25 (gus "N" `unitMaybeExpScalar` 2), "5N^2")
        , (d `exp` b, Right $ Numeric 343 (anonymousNewton `unitMaybeExpScalar` 3), "(7kg*m/s^2)^3")
        , (a `exp` c, Left "Cannot exponentiate by a unit quantity (N). Must exponentiate by a unitless quantity.", "2^(5N)")
        , (b `exp` d, Left "Cannot exponentiate by a unit quantity (((kg*m)/(s^2.0))). Must exponentiate by a unitless quantity.", "3^(7N) -- todo unparenthesize error message")
        ]