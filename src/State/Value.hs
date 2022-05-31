module State.Value where

import Data.Number.CReal
import Unit.Unit
import Utils.Trim

data Value = Numeric CReal (Maybe Unit)
    deriving Eq

instance Show Value where
    show (Numeric quant unit) = trim $ show quant <> " " <> maybe "" show unit
