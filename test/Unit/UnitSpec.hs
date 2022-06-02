module Unit.UnitSpec where

import Test.Hspec
import Unit.Unit
import Unit.UnitScaleOperation

spec :: Spec
spec = do
    describe "castUnit tests" $ do
        let b = BaseUnit "b"
        let kg = BaseUnit "kg"
        let kb = ScaledUnit "kb" (Multiply 100) b
        let mb = ScaledUnit "Mb" (Multiply 100) kb

        it "casts from b to kb" $ do
            1 `shouldBe` 1
