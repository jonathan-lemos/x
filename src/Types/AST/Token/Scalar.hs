module Types.AST.Token.Scalar where

import Data.Number.CReal
import Evaluation.ToValue
import State.Value
import State.XState
import Utils.Either

data Scalar = Number CReal | Variable String
    deriving Eq

instance Show Scalar where
    show (Number n) = show n
    show (Variable x) = show x

instance ToValue Scalar where
    toValue (Number n) _state = Right (Numeric n Nothing)
    toValue (Variable s) state = eitherFromMaybe ("Use of undeclared variable " <> show s) (getVar s state)
