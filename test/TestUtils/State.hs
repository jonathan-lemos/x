module TestUtils.State where

import Data.Number.CReal
import State.XState

mkState :: [(String, CReal)] -> XState
mkState = foldr (\(var, value) state -> putVar state var value) newState