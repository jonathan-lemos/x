module X.TestUtils.State where

import Data.Number.CReal
import X.Data.State.XState
import X.Data.State.Value

mkState :: [(String, Value)] -> XState
mkState = foldr (\(var, value) state -> putVar var value state) newState
