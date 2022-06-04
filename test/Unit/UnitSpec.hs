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
import Unit.UnitScaleOperation
import Unit.UnitLike

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
        , tc "K" "C" (+ 273.15)
        , tc "K" "F" $ (* (9/5)) . (+ 32) . (+ 273.15)
        ]
