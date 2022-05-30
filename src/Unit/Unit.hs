module Unit.Unit where

import Data.Number.CReal
import Unit.UnitLike
import Unit.UnitScaleOperation
import Utils.String
import Unit.Exponential

data Unit
    = BaseUnit String
    | ProductUnit
        { puName :: String
        , puComponents :: [Unit]
        }
    | ScaledUnit
        { suName :: String
        , suOperation :: UnitScaleOperation
        , suBase :: Unit
        }
    deriving (Ord)

instance Show Unit where
    show (BaseUnit base) = show base
    show (ProductUnit name _components) = name
    show (ScaledUnit name _scale _base) = name

instance Eq Unit where
    a == b = toScaleAndBaseUnits a == toScaleAndBaseUnits b

instance UnitLike Unit where
    toScale (BaseUnit _) = 1
    toScale (ProductUnit _name components) = toScale components
    toScale (ScaledUnit _name (Multiply n) base) = toScale base * n
    toScale (ScaledUnit _name (Exponentiate n) base) = toScale base ** n
    toScale (ScaledUnit _name (Add _) base) = toScale base

    toBaseUnits (BaseUnit base) = [Exponential base 1]
    toBaseUnits (ProductUnit _name components) = toBaseUnits components
    toBaseUnits (ScaledUnit _name (Exponentiate n) base) = toBaseUnits $ Exponential base n
    toBaseUnits (ScaledUnit _name (Multiply _) base) = toBaseUnits base
    toBaseUnits (ScaledUnit _name (Add _) base) = toBaseUnits base

_showInfixOp :: (Show a, Show b) => a -> String -> b -> String
_showInfixOp a op b = parenthesize $ show a <> op <> show b

unitless :: Unit
unitless = ProductUnit "" []

unitMult :: Unit -> Unit -> Unit
unitMult a b = ProductUnit (_showInfixOp a "*" b) [a, b]

unitDiv :: Unit -> Unit -> Unit
unitDiv a b = ProductUnit (_showInfixOp a "/" b) [a, unitExpScalar b (-1)]

unitReciprocal :: Unit -> Unit
unitReciprocal u = ScaledUnit (_showInfixOp (1 :: Integer) "/" u) (Exponentiate (-1)) u

unitAddScalar :: Unit -> CReal -> Unit
unitAddScalar u s = ScaledUnit (_showInfixOp u "+" s) (Add s) u

unitSubScalar :: Unit -> CReal -> Unit
unitSubScalar u s = ScaledUnit (_showInfixOp u "-" s) (Add (-s)) u

unitMultScalar :: Unit -> CReal -> Unit
unitMultScalar u s = ScaledUnit (_showInfixOp u "*" s) (Multiply s) u

unitDivScalar :: Unit -> CReal -> Unit
unitDivScalar u s = ScaledUnit (_showInfixOp u "/" s) (Multiply (1 / s)) u

unitExpScalar :: Unit -> CReal -> Unit
unitExpScalar b e = ScaledUnit (_showInfixOp b "^" e) (Exponentiate (-1)) b
