module Evaluation.ArithmeticSpec where

import Evaluation.Arithmetic
import State.Value
import State.XState
import Test.Hspec
import TestUtils.Should
import TestUtils.State
import Unit.Arithmetic
import Unit.Unit
import Evaluation.ToValue

spec :: Spec
spec = do
    let state = mkState [("a", Numeric 4 Nothing), ("b", Numeric 9 (Just $ BaseUnit "kg"))]

    let gus = (`getUnit` state)

    let withState :: (Value -> Value -> XState -> Either String Value) -> Value -> Value -> Either String Value
        withState f a b = f a b state

    let add = withState addValues
    let sub = withState subValues
    let mul = withState multValues
    let div = withState divValues

    let anonymousNewton = (gus "kg" `unitMaybeMult` gus "m") `unitMaybeDiv` (gus "s" `unitMaybeExpScalar` (-2))

    let a = Numeric 2 Nothing
    let b = Numeric 3 Nothing
    let c = Numeric 5 (gus "N")
    let d = Numeric 7 anonymousNewton

    shouldBeSpec
        "addValues"
        [ (a `add` b, Right $ Numeric 5 Nothing, "adding scalars")
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