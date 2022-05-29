{-# LANGUAGE FlexibleInstances #-}
module Unit.BaseUnit where
import Data.Number.CReal
import Unit.Exponential
import Data.Bifunctor
import Data.Foldable

class UnitLike u where
    toQuantityAndBaseUnits :: u -> (CReal, [Exponential String])

    toBaseUnits :: u -> [Exponential BaseUnit]
    toBaseUnits = snd . toQuantityAndBaseUnits

instance UnitLike u => UnitLike (Exponential u) where
    toQuantityAndBaseUnits (Exponential u p) =
        bimap (** p) (fmap (modifyExpPower (*p)) . reduceExpProduct) . toQuantityAndBaseUnits $ u

instance (UnitLike u) => UnitLike [u] where
    toQuantityAndBaseUnits us = foldl'
        (\(aq, au) (cq, cu) -> (aq * cq, reduceExpProduct $ au <> cu))
        (1, [])
        $ toQuantityAndBaseUnits <$> us
