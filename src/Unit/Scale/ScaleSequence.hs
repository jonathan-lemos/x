module Unit.Scale.ScaleSequence where

import Data.Foldable
import Data.Number.CReal
import Unit.Scale.ScaleStep

newtype ScaleSequence = ScaleSequence {steps :: [ScaleStep]}
    deriving (Eq, Ord)

instance Show ScaleSequence where
    show = show . steps

instance Semigroup ScaleSequence where
    a <> b = ScaleSequence (steps a <> steps b)

instance Monoid ScaleSequence where
    mempty = ScaleSequence []

scale :: ScaleSequence -> CReal -> CReal
scale sq n = foldl' (flip applyStep) n (steps sq)

invertScaleSeq :: ScaleSequence -> ScaleSequence
invertScaleSeq = ScaleSequence . fmap invertStep . reverse . steps
