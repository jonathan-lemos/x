module State.XState where

import qualified Data.Map as DM
import Data.Number.CReal
import Unit.Context
import Unit.Prelude
import Unit.ContextUnit
import State.Value

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

putUnit :: ContextUnit -> XState -> XState
putUnit u = _modifyUnits (addUnit u)

putVar :: String -> Value -> XState -> XState
putVar key value = _modifyVariables $ DM.insert key value

getVar :: String -> XState -> Maybe Value
getVar s = DM.lookup s . variables

getUnit :: String -> XState -> Maybe ContextUnit
getUnit u = getUnitByName u . units
