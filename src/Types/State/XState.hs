module Types.State.XState where

import qualified Data.Map as DM
import Data.Number.CReal
import Types.Unit.Unit

data XState = XState {
    variables :: DM.Map String CReal,
    units :: DM.Map String Unit
}

mapVariables :: (DM.Map String CReal -> DM.Map String CReal) -> XState -> XState
mapVariables f s = s { variables = f $ variables s }

mapUnits :: (DM.Map String Unit -> DM.Map String Unit) -> XState -> XState
mapUnits f s = s { units = f $ units s }

newState :: XState
newState = XState {
    variables = DM.empty,
    units = DM.empty
}

putVar :: XState -> String -> CReal -> XState
putVar s v n = s {
    variables = DM.insert v n (variables s)
}

getVar :: XState -> String -> Maybe CReal
getVar s v = DM.lookup v (variables s)
