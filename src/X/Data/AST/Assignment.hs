module X.Data.AST.Assignment where

import Text.Printf
import X.Data.AST.Arithmetic

data Assignment = Assignment { to :: String, value :: AdditiveChain }
    deriving Eq

instance Show Assignment where
    show (Assignment to value) = printf "%s <- %s" (show to) (show value)
