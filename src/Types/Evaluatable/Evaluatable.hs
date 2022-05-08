module Types.Evaluatable.Evaluatable where

import Data.Number.CReal
import Shell.State

class Evaluatable e where
    evaluate :: e -> XState -> Either String CReal

