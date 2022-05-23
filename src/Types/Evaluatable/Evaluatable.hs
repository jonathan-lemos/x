module Types.Evaluatable.Evaluatable where

import Data.Number.CReal
import State.XState

class Evaluatable e where
    evaluate :: e -> XState -> Either String CReal

