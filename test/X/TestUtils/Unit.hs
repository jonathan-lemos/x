{-# LANGUAGE FlexibleInstances #-}
module X.TestUtils.Unit where

import X.Data.Unit.Prelude
import X.Data.Unit.Unit
import Data.Foldable
import X.Utils.Function
import X.TestUtils.Unwrap
import Data.Number.CReal
import X.Data.Unit.Arithmetic

class TestUnitConvertible a where
    toUnit :: a -> Unit

instance TestUnitConvertible Unit where
    toUnit = id

instance TestUnitConvertible String where
    toUnit = defaultUnit

defaultUnit :: String -> Unit
defaultUnit s = find ((== s) . show) preludeUnits >$ unwrap

castUnitOrDie :: (TestUnitConvertible a, TestUnitConvertible b) => a -> b -> (CReal -> CReal)
castUnitOrDie a b = castUnit (toUnit a) (toUnit b) >$ unwrap

testUnitConvertibleIfy :: (TestUnitConvertible a, TestUnitConvertible b) => (Unit -> Unit -> c) -> (a -> b -> c)
testUnitConvertibleIfy f a b = f (toUnit a) (toUnit b)

testUnitMult :: (TestUnitConvertible a, TestUnitConvertible b) => a -> b -> Unit
testUnitMult = testUnitConvertibleIfy unitMult

testUnitDiv :: (TestUnitConvertible a, TestUnitConvertible b) => a -> b -> Unit
testUnitDiv = testUnitConvertibleIfy unitDiv

testUnitExpScalar ::(TestUnitConvertible a) => a -> CReal -> Unit
testUnitExpScalar b e = toUnit b `unitExpScalar` e
