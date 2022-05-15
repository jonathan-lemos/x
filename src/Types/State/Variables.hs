module Types.State.Variables where

import qualified Data.Map as DM
import Types.State.XState
import Data.Number.CReal

putVar :: String -> CReal -> XState -> XState
putVar v n = mapVariables $ DM.insert v n

getVar :: String -> XState -> Maybe CReal
getVar v = DM.lookup v . variables
