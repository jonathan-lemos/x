module X.Data.State.XState where

import qualified Data.Map as DM
import X.Data.Unit.Context
import X.Data.Unit.Prelude
import X.Data.Unit.Unit
import X.Data.State.Value

data XState = XState {
    variables :: DM.Map String Value,
    units :: UnitContext
}

_modifyVariables :: (DM.Map String Value -> DM.Map String Value) -> XState -> XState
_modifyVariables f s = s { variables = f $ variables s }

_modifyUnits :: (UnitContext -> UnitContext) -> XState -> XState
_modifyUnits f s = s { units = f $ units s }

newState :: XState
newState = XState {
    variables = DM.empty,
    units = newContext preludeUnits
}

putUnit :: Unit -> XState -> XState
putUnit u = _modifyUnits (addUnit u)

putVar :: String -> Value -> XState -> XState
putVar key value = _modifyVariables $ DM.insert key value

getVar :: String -> XState -> Maybe Value
getVar s = DM.lookup s . variables

getUnit :: String -> XState -> Maybe Unit
getUnit u = getUnitByName u . units
