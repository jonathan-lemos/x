module State.XState where

import qualified Data.Map as DM
import Data.Number.CReal
import Unit.Context
import Unit.Prelude
import Unit.ContextUnit

data XState = XState {
    variables :: DM.Map String CReal,
    units :: UnitContext
}

modifyVariables :: (DM.Map String CReal -> DM.Map String CReal) -> XState -> XState
modifyVariables f s = s { variables = f $ variables s }

modifyUnits :: (UnitContext -> UnitContext) -> XState -> XState
modifyUnits f s = s { units = f $ units s }

newState :: XState
newState = XState {
    variables = DM.empty,
    units = newContext preludeUnits
}

putUnit :: XState -> ContextUnit -> XState
putUnit s u = modifyUnits (addUnit u) s

getUnit :: XState -> String -> Maybe ContextUnit
getUnit s u = getUnitByName u (units s)

putVar :: XState -> String -> CReal -> XState
putVar s v n = s {
    variables = DM.insert v n (variables s)
}

getVar :: XState -> String -> Maybe CReal
getVar s v = DM.lookup v (variables s)
