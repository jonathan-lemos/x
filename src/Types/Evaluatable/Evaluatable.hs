module Types.Evaluatable.Evaluatable where

import Data.Number.CReal
import Types.State

class Evaluatable e where
    evaluate :: e -> XState -> Either String CReal

