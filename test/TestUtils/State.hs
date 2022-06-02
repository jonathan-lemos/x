module TestUtils.State where

import Data.Number.CReal
import State.XState
import State.Value

mkState :: [(String, Value)] -> XState
mkState = foldr (\(var, value) state -> putVar var value state) newState
