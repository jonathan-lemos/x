module Types.AST.Token.Scalar where

import Data.Number.CReal
import Evaluation.ToValue
import State.Value
import State.XState
import Utils.Either

data Scalar = Number CReal | Variable String

instance ToValue Scalar where
    toValue (Number n) _state = Right (Numeric n [])
    toValue (Variable s) state = eitherFromMaybe ("Unknown variable " <> show s <> "used") (getVar s state)
