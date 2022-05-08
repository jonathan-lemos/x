module Types.Evaluatable.Evaluatable where

import Data.Number.CReal
import State.State

class Evaluatable e where
    evaluate :: e -> XState -> Either String CReal

