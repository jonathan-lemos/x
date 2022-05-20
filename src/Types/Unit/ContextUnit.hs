module Types.Unit.ContextUnit where

import Data.Bifunctor
import qualified Data.Map as DM
import Data.Number.CReal
import Types.Unit.BaseUnit
import Types.Unit.Exponential
import Data.Foldable

data ContextUnit
    = ContextBaseUnit BaseUnit
    | ContextDerivedUnit
        { cduName :: String
        , cduQuantity :: CReal
        , cduComponents :: [Exponential ContextUnit]
        }
    deriving (Ord)

instance Show ContextUnit where
    show (ContextBaseUnit bu) = show bu
    show (ContextDerivedUnit name quantity components) = name

instance Eq ContextUnit where
    a == b = toBaseUnitsAndQuantity a == toBaseUnitsAndQuantity b

instance UnitClass ContextUnit where
    toBaseUnitsAndQuantity (ContextBaseUnit bu) = toBaseUnitsAndQuantity bu
    toBaseUnitsAndQuantity (ContextDerivedUnit _ quantity components) =
        let componentBaseUnits = (\(Exponential b e) -> second (fmap $ modifyExpPower (* e)) $ toBaseUnitsAndQuantity b) <$> components
            totalQuantity = product $ fst <$> componentBaseUnits
            totalBaseUnits = foldl' mergeExponentials [] $ snd <$> componentBaseUnits
         in (totalQuantity, totalBaseUnits)
