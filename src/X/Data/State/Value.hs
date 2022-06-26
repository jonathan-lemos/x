module X.Data.State.Value where

import Data.Number.CReal
import X.Data.Unit.Unit
import X.Utils.Trim

data Value = Numeric CReal (Maybe Unit)
    deriving Eq

instance Show Value where
    show (Numeric quant unit) = trim $ show quant <> " " <> maybe "" show unit
