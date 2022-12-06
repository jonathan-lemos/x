module X.Data.Operator where
import Data.Number.CReal
import X.Utils.CReal

data AdditiveOperator = Add | Sub
    deriving (Eq, Ord, Enum, Show)

data MultiplicativeOperator = Mul | Div
    deriving (Eq, Ord, Enum, Show)

class OperatorLike op where
    applyOp :: op -> CReal -> CReal -> CReal

class ChainHeadOp op where
    chainHeadOp :: op

instance OperatorLike AdditiveOperator where
    applyOp Add = (+)
    applyOp Sub = (-)

instance ChainHeadOp AdditiveOperator where
    chainHeadOp = Add

instance OperatorLike MultiplicativeOperator where
    applyOp Mul = (*)
    applyOp Div = safeDiv

instance ChainHeadOp MultiplicativeOperator where
    chainHeadOp = Mul

