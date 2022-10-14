module X.Data.Operator where

import Data.Number.CReal
import X.Utils.CReal
import X.Data.Display

class OperatorLike a where
    applyOp :: a -> (CReal -> CReal -> CReal)

data AdditiveOperator = Add | Sub
    deriving (Eq, Ord, Enum, Show)

instance Display AdditiveOperator where
    display Add = "+"
    display Sub = "-"

instance OperatorLike AdditiveOperator where
    applyOp Add = (+)
    applyOp Sub = (-)

data MultiplicativeOperator = Mul | Div
    deriving (Eq, Ord, Enum, Show)

instance Display MultiplicativeOperator where
    display Mul = "*"
    display Div = "/"

instance OperatorLike MultiplicativeOperator where
    applyOp Mul = (*)
    applyOp Div = safeDiv

data RightAssociativeOperator = Exp
    deriving (Eq, Ord, Enum)

