module Unit.Context where

import Data.Foldable
import Data.List
import qualified Data.Map as DM
import Data.Number.CReal
import qualified Structure.BiMap as BM
import Unit.BaseUnit
import Unit.ContextUnit
import Unit.Exponential

data UnitContext = UnitContext
    { baseUnitsToContextUnit :: BM.BiMap [Exponential BaseUnit] ContextUnit
    , nameToContextUnit :: DM.Map String ContextUnit
    }

modifyBaseUnitsToContextUnit :: (BM.BiMap [Exponential BaseUnit] ContextUnit -> BM.BiMap [Exponential BaseUnit] ContextUnit) -> UnitContext -> UnitContext
modifyBaseUnitsToContextUnit f ctx = ctx{baseUnitsToContextUnit = f $ baseUnitsToContextUnit ctx}

modifyNameToContextUnit :: (DM.Map String ContextUnit -> DM.Map String ContextUnit) -> UnitContext -> UnitContext
modifyNameToContextUnit f ctx = ctx{nameToContextUnit = f $ nameToContextUnit ctx}

emptyContext :: UnitContext
emptyContext = UnitContext BM.empty DM.empty

newContext :: [ContextUnit] -> UnitContext
newContext = foldl' (flip addUnit) emptyContext

addUnit :: ContextUnit -> UnitContext -> UnitContext
addUnit u =
    modifyNameToContextUnit (DM.insert (show u) u)
        . modifyBaseUnitsToContextUnit (BM.insert (toBaseUnits u) u)

getUnitByName :: String -> UnitContext -> Maybe ContextUnit
getUnitByName s = DM.lookup s . nameToContextUnit

castUnit :: (Show a, UnitLike a, Show b, UnitLike b) => a -> b -> Either String (CReal -> CReal)
castUnit a b =
    let (aQuantity, aBaseUnits) = toQuantityAndBaseUnits a
        (bQuantity, bBaseUnits) = toQuantityAndBaseUnits b
     in if sort aBaseUnits == sort bBaseUnits
            then Right $ (/ bQuantity) . (* aQuantity)
            else Left $ show a <> " cannot be converted to " <> show b

castMaybeUnit :: (Show a, UnitLike a, Show b, UnitLike b) => Maybe a -> Maybe b -> Either String (CReal -> CReal)
castMaybeUnit Nothing Nothing = Right id
castMaybeUnit (Just a) (Just b) = castUnit a b
castMaybeUnit a b =
    let unitName = maybe "(unitless quantity)" show
        in Left $ unitName a <> " cannot be converted to " <> unitName b

