module X.Data.AST.UnitExpressionSpec where

import Data.List
import Data.Maybe
import Test.Hspec
import X.Data.AST.UnitExpression
import X.Data.State.XState
import X.Data.Unit.Arithmetic
import X.TestUtils.Either
import X.TestUtils.Should
import X.Utils.Function
import X.TestUtils.Unit
import Harness.TestCase
import X.Control.Try
import Harness.With
import X.TestUtils.Unwrap

spec :: Spec
spec = do
    let justUnitCase s = ufToUnit (JustUnit s) newState `shouldEq` (toUnit s >$ Success) `withTitle` s
    let unitPowerCase base exponent = ufToUnit (UnitPower base exponent) newState `shouldEq` (toUnit base `unitExpScalar` exponent >$ Success) `withTitle` (base <> "^" <> show exponent)

    desc "ufToUnit" $ do
        justUnitCase "kg"
        justUnitCase "m"
        unitPowerCase "m" 2
        unitPowerCase "m" (-2)

    let ume xs = UnitMultExpression (head xs) (tail xs)
    let umeCase us = unwrap (umToUnit (ume us) newState) `shouldEq` unitProduct (unwrap . (`ufToUnit` newState) <$> us) `withTitle` intercalate ", " (show <$> us)

    desc "umToUnit" $ do
        umeCase [JustUnit "kg"]
        umeCase [JustUnit "kg", JustUnit "m"]
        umeCase [JustUnit "kg", UnitPower "m" 2]
        umeCase [JustUnit "kg", UnitPower "m" 2, UnitPower "s" (-2)]


    let ueFractionCase n d = unwrap (ueToUnit (UnitFraction n d) newState) `shouldEq` (unwrap (umToUnit n newState) `unitDiv` unwrap (umToUnit d newState)) `withTitle` (show n <> "/" <> show d)
    let ueProductCase n = ueToUnit (UnitProduct n) newState `shouldEq` umToUnit n newState `withTitle` show n

    desc "ueToUnit" $ do
        ueFractionCase (ume [JustUnit "m"]) (ume [JustUnit "s"])
        ueFractionCase (ume [JustUnit "m", UnitPower "kg" 2]) (ume [UnitPower "s" 2])
        ueProductCase (ume [JustUnit "m", UnitPower "kg" 2])
        ueProductCase (ume [JustUnit "m"])