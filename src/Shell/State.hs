module Shell.State where

import qualified Data.Map as DM
import Data.Number.CReal

newtype XState = XState {
    variables :: DM.Map String CReal
}

putVar :: XState -> String -> CReal -> XState
putVar s v n = s {
    variables = DM.insert v n (variables s)
}

getVar :: XState -> String -> Maybe CReal
getVar s v = DM.lookup v (variables s)
