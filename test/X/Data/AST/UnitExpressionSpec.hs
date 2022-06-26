module X.Data.AST.UnitExpressionSpec where

import Data.List
import Data.Maybe
import Test.Hspec
import X.Data.AST.UnitExpression
import X.Data.State.XState
import X.Data.Unit.Arithmetic
import X.TestUtils.Either
import X.TestUtils.Should

spec :: Spec
spec = do
    let state = newState
    let gu s = fromJust $ getUnit s state

    let juCase s = (ufToUnit (JustUnit s) state, Right . gu $ s, s)
    let upCase b e = (ufToUnit (UnitPower b e) state, Right $ gu b `unitExpScalar` e, b <> "^" <> show e)

    shouldBeSpec
        "ufToUnit"
        [ juCase "kg"
        , juCase "m"
        , upCase "m" 2
        , upCase "m" (-2)
        ]

    let ume xs = UnitMultExpression (head xs) (tail xs)
    let umeCase us = (right $ umToUnit (ume us) state, unitProduct (right . (`ufToUnit` state) <$> us), intercalate ", " $ show <$> us)

    shouldBeSpec
        "umToUnit"
        [ umeCase [JustUnit "kg"]
        , umeCase [JustUnit "kg", JustUnit "m"]
        , umeCase [JustUnit "kg", UnitPower "m" 2]
        , umeCase [JustUnit "kg", UnitPower "m" 2, UnitPower "s" (-2)]
        ]

    let ueFractionCase n d = (right $ ueToUnit (UnitFraction n d) state, right (umToUnit n state) `unitDiv` right (umToUnit d state), show n <> "/" <> show d)
    let ueProductCase n = (right $ ueToUnit (UnitProduct n) state, right $ umToUnit n state, show n)

    shouldBeSpec
        "ueToUnit"
        [ ueFractionCase (ume [JustUnit "m"]) (ume [JustUnit "s"])
        , ueFractionCase (ume [JustUnit "m", UnitPower "kg" 2]) (ume [UnitPower "s" 2])
        , ueProductCase (ume [JustUnit "m", UnitPower "kg" 2])
        , ueProductCase (ume [JustUnit "m"])
        ]