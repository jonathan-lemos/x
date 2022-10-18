module X.Data.Operator where
import Data.Number.CReal
import X.Utils.CReal

data AdditiveOperator = Add | Sub
    deriving (Eq, Ord, Enum, Show)

data MultiplicativeOperator = Mul | Div
    deriving (Eq, Ord, Enum, Show)

class OperatorLike op where
    applyOp :: op -> CReal -> CReal -> CReal

instance OperatorLike AdditiveOperator where
    applyOp Add = (+)
    applyOp Sub = (-)

instance OperatorLike MultiplicativeOperator where
    applyOp Mul = (*)
    applyOp Div = safeDiv

