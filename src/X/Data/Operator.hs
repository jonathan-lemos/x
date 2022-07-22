module X.Data.Operator where

data AdditiveOperator = Add | Sub
    deriving (Eq, Ord, Enum)

data MultiplicativeOperator = Mul | Div
    deriving (Eq, Ord, Enum)

instance Show AdditiveOperator where
    show Add = "+"
    show Sub = "-"
instance Show MultiplicativeOperator where
    show Mul = "*"
    show Div = "/"
