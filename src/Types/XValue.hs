module Types.XValue where

import Data.Number.CReal

data XValue = XNumber CReal | XVariable String
    deriving Eq


instance Show XValue where
    show (XNumber n) = show n
    show (XVariable v) = v
