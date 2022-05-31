module Unit.Context where

import Data.Foldable
import qualified Data.Map as DM
import Unit.Unit

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
