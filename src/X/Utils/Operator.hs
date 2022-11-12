module X.Utils.Operator where

import X.Data.Operator
import Data.Foldable
import Data.Number.CReal

evalOperators :: (Foldable f, OperatorLike o) => f (o, CReal) -> CReal -> CReal
evalOperators xs n =
    foldl' (\result (o, n) -> applyOp o result n) n xs

