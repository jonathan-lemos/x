module Types.Unit.ContextUnit where

import Data.Bifunctor
import qualified Data.Map as DM
import Data.Number.CReal
import Types.Unit.BaseUnit
import Types.Unit.Exponential
import Data.Foldable
import Types.Unit.Scale.ScaleSequence

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
    show (ProductUnit name components) = name
    show (ScaledUnit name scale base) = name

instance Eq ContextUnit where
    a == b = toBaseUnitsAndQuantity a == toBaseUnitsAndQuantity b

instance UnitClass ContextUnit where
    toBaseUnitsAndQuantity (CtxBaseUnit base) = toBaseUnitsAndQuantity base
    toBaseUnitsAndQuantity (ProductUnit name components) =
        let componentBaseUnits = (\(Exponential b e) -> second (fmap $ modifyExpPower (* e)) $ toBaseUnitsAndQuantity b) <$> components
            totalQuantity = product $ fst <$> componentBaseUnits
            totalBaseUnits = foldl' mergeExpProducts [] $ snd <$> componentBaseUnits
         in (totalQuantity, totalBaseUnits)
    toBaseUnitsAndQuantity (ScaledUnit name scaler base) = first (scale scaler) $ toBaseUnitsAndQuantity base

