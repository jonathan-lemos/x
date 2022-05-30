module Unit.Context where

import Data.Foldable
import Data.List
import qualified Data.Map as DM
import Data.Number.CReal
import Unit.Unit
import Unit.Exponential
import Unit.UnitLike

newtype UnitContext = UnitContext
    { nameToContextUnit :: DM.Map String Unit
    }

modifyNameToContextUnit :: (DM.Map String Unit -> DM.Map String Unit) -> UnitContext -> UnitContext
modifyNameToContextUnit f ctx = ctx{nameToContextUnit = f $ nameToContextUnit ctx}

emptyContext :: UnitContext
emptyContext = UnitContext DM.empty

newContext :: [Unit] -> UnitContext
newContext = foldl' (flip addUnit) emptyContext

addUnit :: Unit -> UnitContext -> UnitContext
addUnit u =
    modifyNameToContextUnit (DM.insert (show u) u)

getUnitByName :: String -> UnitContext -> Maybe Unit
getUnitByName s = DM.lookup s . nameToContextUnit

castUnit :: (Show a, UnitLike a, Show b, UnitLike b) => a -> b -> Either String (CReal -> CReal)
castUnit a b =
    let (aQuantity, aBaseUnits) = toQuantityAndBaseUnits a
        (bQuantity, bBaseUnits) = toQuantityAndBaseUnits b
     in if sort aBaseUnits == sort bBaseUnits
            then Right $ (/ bQuantity) . (* aQuantity)
            else Left $ show a <> " cannot be converted to " <> show b
