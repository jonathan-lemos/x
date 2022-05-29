module Unit.ContextUnit where

import Data.Bifunctor
import Unit.BaseUnit
import Unit.Exponential
import Data.Foldable
import Unit.Scale.ScaleSequence

data ContextUnit
    = CtxBaseUnit BaseUnit
    | ProductUnit
        { puName :: String
        , puComponents :: [Exponential ContextUnit]
        }
    | ScaledUnit
        { suName :: String,
          suScaler :: ScaleSequence
        , suBase :: ContextUnit}
        deriving (Ord)

instance Show ContextUnit where
    show (CtxBaseUnit base) = show base
    show (ProductUnit name _components) = name
    show (ScaledUnit name _scale _base) = name

instance Eq ContextUnit where
    a == b = toQuantityAndBaseUnits a == toQuantityAndBaseUnits b

instance UnitLike ContextUnit where
    toQuantityAndBaseUnits (CtxBaseUnit base) = toQuantityAndBaseUnits base
    toQuantityAndBaseUnits (ProductUnit _name components) =
        let componentQuantityAndBaseUnits = toQuantityAndBaseUnits <$> components
            totalQuantity = product $ fst <$> componentQuantityAndBaseUnits
            totalBaseUnits = foldl' mergeExpProducts [] $ snd <$> componentQuantityAndBaseUnits
         in (totalQuantity, totalBaseUnits)
    toQuantityAndBaseUnits (ScaledUnit _name scaler base) = first (scale scaler) $ toQuantityAndBaseUnits base
