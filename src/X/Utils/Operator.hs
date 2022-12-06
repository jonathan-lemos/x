module X.Utils.Operator where

import X.Data.Operator
import Data.Foldable
import Data.Number.CReal

evalOperators :: (Foldable f, OperatorLike o) => CReal -> f (o, CReal) -> CReal
evalOperators =
    foldl' (\result (o, n) -> applyOp o result n)

