{-# LANGUAGE FlexibleInstances #-}
module Unit.UnitLike where
import Unit.Exponential
import Control.Applicative
import Data.Number.CReal

class UnitLike u where
    toScaleAndBaseUnits :: u -> (CReal, [Exponential String])
    toScaleAndBaseUnits = liftA2 (,) toScale toBaseUnits

    toBaseUnits :: u -> [Exponential String]
    toBaseUnits = snd . toScaleAndBaseUnits

    toScale :: u -> CReal
    toScale = fst . toScaleAndBaseUnits

instance UnitLike u => UnitLike (Exponential u) where
    toScale (Exponential u p) = toScale u ** p

    toBaseUnits (Exponential u p) = reduceExpProduct $ modifyExpPower (*p) <$> toBaseUnits u

instance (UnitLike u) => UnitLike [u] where
    toBaseUnits = reduceExpProduct . concatMap toBaseUnits

    toScale us = product $ toScale <$> us
