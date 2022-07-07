{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module X.Data.Unit.UnitSpec where

import Data.Either
import Data.Foldable (find)
import Data.Maybe
import Test.Hspec
import X.TestUtils.Arbitrary
import X.TestUtils.Should
import X.Data.Unit.Arithmetic
import X.Data.Unit.Prelude
import X.Data.Unit.Unit
import X.Data.Unit.UnitLike
import X.Data.Unit.UnitScaleOperation
import X.Utils.Function
import X.TestUtils.Unwrap
import Harness.With
import Harness.TestCase
import Harness.QcCase
import X.TestUtils.Unit
import X.TestUtils.Try
import Data.Number.CReal

spec :: Spec
spec = parallel $ do
    let castTo :: (TestUnitConvertible a, TestUnitConvertible b) => a -> b -> CReal -> CReal
        castTo = castUnitOrDie

    let anonymousNewton = ("kg" `testUnitMult` "m") `testUnitDiv` ("s" `testUnitExpScalar` 2)

    desc "toScaleAndBaseUnitTests" $ do
        toScaleAndBaseUnits anonymousNewton `shouldEq` toScaleAndBaseUnits (toUnit "N") `withTitle` "anonymous newton == newton"

    qcDesc "castUnit quickchecks" $ do
        "b" `castTo` "kb" `shouldQcEq` (/ 1000)
        "kb" `castTo` "b" `shouldQcEq` (* 1000)
        anonymousNewton `castTo` "N" `shouldQcEq` id
        "N" `castTo` anonymousNewton `shouldQcEq` id
        "K" `castTo` "C" `shouldQcEq` \x -> x - 273.15
        "C" `castTo` "F" `shouldQcEq` \x -> x * (9 / 5) + 32
        "K" `castTo` "F" `shouldQcEq` \x -> (x - 273.15) * (9 / 5) + 32
        ("N" `testUnitMult` "m") `castTo` ("N" `testUnitMult` "cm") `shouldQcEq` (* 100)
        ("N" `testUnitMult` "cm") `castTo` ("N" `testUnitMult` "m") `shouldQcEq` (/ 100)
        ("N" `testUnitDiv` "m") `castTo` ("N" `testUnitDiv` "cm") `shouldQcEq` (/ 100)
        ("N" `testUnitDiv` "cm") `castTo` ("N" `testUnitDiv` "m") `shouldQcEq` (* 100)
        ("N" `testUnitMult` "C") `castTo` ("N" `testUnitMult` "K") `shouldQcEq` id
        ("N" `testUnitMult` "K") `castTo` ("N" `testUnitMult` "C") `shouldQcEq` id
        ("N" `testUnitDiv` "C") `castTo` ("N" `testUnitDiv` "K") `shouldQcEq` id
        ("N" `testUnitDiv` "K") `castTo` ("N" `testUnitDiv` "C") `shouldQcEq` id
        ("km" `testUnitExpScalar` 2) `castTo` ("m" `testUnitExpScalar` 2) `shouldQcEq` (* 1000000)
        ("m" `testUnitExpScalar` 2) `castTo` ("km" `testUnitExpScalar` 2) `shouldQcEq` (/ 1000000)
