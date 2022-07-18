module X.Data.Operator where
import Data.Number.CReal
import X.Utils.CReal

data Operator = Add | Sub | Mul | Div | Exp
    deriving Eq

instance Show Operator where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"

applyOp :: Operator -> CReal -> CReal -> CReal
applyOp Add = (+)
applyOp Sub = (-)
applyOp Mul = (*)
applyOp Div = safeDiv
applyOp Exp = safeExp
