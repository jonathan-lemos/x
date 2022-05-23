module Unit.Scale.ScaleStep where

import Data.Number.CReal

data ScaleStep = ScaleMultiply CReal | ScaleAdd CReal
    deriving (Eq, Ord)

applyStep :: ScaleStep -> CReal -> CReal
applyStep (ScaleMultiply c) = (* c)
applyStep (ScaleAdd x) = (+ x)

invertStep :: ScaleStep -> ScaleStep
invertStep (ScaleAdd x) = ScaleAdd (negate x)
invertStep (ScaleMultiply c) = ScaleMultiply (1 / c)

instance Show ScaleStep where
    show (ScaleMultiply n) = "* " <> show n
    show (ScaleAdd n) = "+ " <> show n
