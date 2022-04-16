module Types.Expression where

import Data.Number.CReal

class Expression e where
    evaluate :: e -> CReal

