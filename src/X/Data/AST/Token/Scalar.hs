module X.Data.AST.Token.Scalar where

import Data.Number.CReal
import X.Evaluation.ToValue
import X.Data.State.Value
import X.Data.State.XState
import X.Control.Try
import X.Utils.Try

-- | A unitless quantity, a number or a variable
data Scalar = Number CReal | Variable String
    deriving Eq

instance Show Scalar where
    show (Number n) = show n
    show (Variable x) = show x

instance ToValue Scalar where
    toValue (Number n) _state = Success (Numeric n Nothing)
    toValue (Variable s) state = maybeToTry ("Use of undeclared variable " <> show s) (getVar s state)
