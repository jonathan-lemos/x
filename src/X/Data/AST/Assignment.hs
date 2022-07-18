module X.Data.AST.Assignment where
import X.Data.Value
import Text.Printf

data Assignment = Assignment { to :: String, value :: Value }
    deriving Eq

instance Show Assignment where
    show (Assignment to value) = printf "%s <- %s" (show to) (show value)
