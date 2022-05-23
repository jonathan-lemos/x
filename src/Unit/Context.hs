module Unit.Context where

import Control.Applicative
import Data.Foldable
import Data.List
import qualified Data.Map as DM
import Data.Maybe
import Data.Number.CReal
import qualified Structure.BiMap as BM
import Structure.Graph
import Unit.BaseUnit
import Unit.ContextUnit
import Unit.Exponential
import Unit.Scale.ScaleSequence
import Unit.ValueUnit
import Utils.Map

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

castUnit :: ContextUnit -> ContextUnit -> Maybe (CReal -> CReal)
castUnit a b =
    let (aQuantity, aBaseUnits) = toBaseUnitsAndQuantity a
        (bQuantity, bBaseUnits) = toBaseUnitsAndQuantity b
     in if sort aBaseUnits == sort bBaseUnits
            then Just $ (/ bQuantity) . (* aQuantity)
            else Nothing
