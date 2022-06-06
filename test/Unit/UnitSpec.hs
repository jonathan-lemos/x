{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Unit.UnitSpec where

import Data.Either
import Data.Foldable (find)
import Data.Maybe
import Test.Hspec
import TestUtils.Arbitrary
import TestUtils.Should
import Unit.Arithmetic
import Unit.Prelude
import Unit.Unit
import Unit.UnitLike
import Unit.UnitScaleOperation

spec :: Spec
spec = parallel $ do
    let units = preludeUnits
    let lookup s = fromJust $ find ((== s) . show) units

    let right (Right x) = x
        right (Left e) = error e

    let cu a b = right $ castUnit a b

    let clu a b = cu (lookup a) (lookup b)

    let tc a b f = (clu a b, f, show a <> " -> " <> show b)

    let anonymousNewton = (lookup "kg" `unitMult` lookup "m") `unitDiv` (lookup "s" `unitExpScalar` 2)

    shouldBeSpec
        "toScaleAndBaseUnitTests"
        [(toScaleAndBaseUnits anonymousNewton, toScaleAndBaseUnits $ lookup "N", "anonymous newton == newton")]

    shouldBeQcSpec
        "castUnit tests"
        [ tc "b" "kb" (/ 1000)
        , tc "kb" "b" (* 1000)
        , (cu anonymousNewton (lookup "N"), id, "anonymous newton -> newton")
        , tc "K" "C" (+ negate 273.15)
        , tc "C" "F" $ (+ 32) . (* (9 / 5))
        , tc "K" "F" $ (+ 32) . (* (9 / 5)) . (+ negate 273.15)
        , (cu (lookup "N" `unitMult` lookup "m") (lookup "N" `unitMult` lookup "cm"), (* 100), "N*m -> N*cm")
        , (cu (lookup "N" `unitMult` lookup "cm") (lookup "N" `unitMult` lookup "m"), (/ 100), "N*cm -> N*m")
        , (cu (lookup "N" `unitDiv` lookup "m") (lookup "N" `unitDiv` lookup "cm"), (/ 100), "N/m -> N/cm")
        , (cu (lookup "N" `unitDiv` lookup "cm") (lookup "N" `unitDiv` lookup "m"), (* 100), "N/cm -> N/m")
        , (cu (lookup "N" `unitMult` lookup "C") (lookup "N" `unitMult` lookup "K"), id, "N*C -> N*K")
        , (cu (lookup "N" `unitMult` lookup "C") (lookup "N" `unitMult` lookup "K"), id, "N*K -> N*C")
        , (cu (lookup "km" `unitExpScalar` 2) (lookup "m" `unitExpScalar` 2), (* 1000000), "km^2 -> m^2")
        ]
