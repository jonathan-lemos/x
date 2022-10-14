module X.Data.Operator where
import Data.Number.CReal
import X.Utils.CReal

class OperatorLike a where
    applyOp :: a -> (CReal -> CReal -> CReal)

data AdditiveOperator = Add | Sub
    deriving (Eq, Ord, Enum)

instance Show AdditiveOperator where
    show Add = "+"
    show Sub = "-"

instance OperatorLike AdditiveOperator where
    applyOp Add = (+)
    applyOp Sub = (-)

data MultiplicativeOperator = Mul | Div
    deriving (Eq, Ord, Enum)

instance Show MultiplicativeOperator where
    show Mul = "*"
    show Div = "/"


instance OperatorLike MultiplicativeOperator where
    applyOp Mul = (*)
    applyOp Div = safeDiv

data RightAssociativeOperator = Exp
    deriving (Eq, Ord, Enum)

instance OperatorLike RightAssociativeOperator where
    applyOp Exp = safeExp

instance Show RightAssociativeOperator where
    show Exp = "^"
