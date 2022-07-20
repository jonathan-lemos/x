module X.Data.Operator where
import Data.Number.CReal
import X.Utils.CReal

data AdditiveOperator = Add | Sub
    deriving Eq

data MultiplicativeOperator = Mul | Div
    deriving Eq

instance Show AdditiveOperator where
    show Add = "+"
    show Sub = "-"
instance Show MultiplicativeOperator where
    show Mul = "*"
    show Div = "/"
