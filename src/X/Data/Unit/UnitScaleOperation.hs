module X.Data.Unit.UnitScaleOperation where

import Data.Number.CReal
import X.Utils.CReal

data UnitScaleOperation = Add CReal | Multiply CReal | Exponentiate CReal
    deriving (Eq, Ord)

instance Show UnitScaleOperation where
    show (Add n) =
        if n >= 0
            then "+ " <> show n
            else "- " <> show (negate n)
    show (Multiply n) = "* " <> show n
    show (Exponentiate n) = "^ " <> show n

invertUso :: UnitScaleOperation -> UnitScaleOperation
invertUso (Add n) = Add (-n)
invertUso (Multiply n) = Multiply (1 `safeDiv` n)
invertUso (Exponentiate n) = Exponentiate (1 `safeDiv` n)

applyUso :: UnitScaleOperation -> CReal -> CReal
applyUso (Add n) = (+n)
applyUso (Multiply n) = (*n)
applyUso (Exponentiate n) = (`absExp` n)

applyUsos :: Foldable f => f UnitScaleOperation -> CReal -> CReal
applyUsos = foldr ((.) . applyUso) id
