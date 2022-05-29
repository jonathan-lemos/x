module State.Value where

import Data.Number.CReal
import Unit.ValueUnit

data Value = Numeric CReal (Maybe ValueUnit)
    deriving Eq

instance Show Value where
    show (Numeric quant unit) = show quant <> maybe "" ((<> " ") . show) unit

