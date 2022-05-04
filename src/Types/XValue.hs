module Types.XValue where

import Data.Number.CReal

data XValue = XNumber CReal | XVariable String
