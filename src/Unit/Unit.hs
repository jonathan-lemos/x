module Unit.Unit where

import Data.Number.CReal
import Unit.UnitLike
import Unit.UnitScaleOperation
import Utils.String
import Unit.Exponential
import Data.List

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
    show (BaseUnit base) = base
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

unitMaybeify :: (Show a, UnitLike a, Show b, UnitLike b) => c -> (a -> b -> Either String c) -> Maybe a -> Maybe b -> Either String c
unitMaybeify _def f (Just a) (Just b) = f a b
unitMaybeify def _f Nothing Nothing = Right def
unitMaybeify _def _f (Just a) Nothing = Left $ show a <> " cannot be converted to unitless quantity"
unitMaybeify _def _f Nothing (Just b) = Left $ "unitless quantity cannot be converted to " <> show b

castUnit :: (Show a, UnitLike a, Show b, UnitLike b) => a -> b -> Either String (CReal -> CReal)
castUnit a b =
    let (aScale, aBaseUnits) = toScaleAndBaseUnits a
        (bScale, bBaseUnits) = toScaleAndBaseUnits b
     in if sort aBaseUnits == sort bBaseUnits
            then Right $ (/ bScale) . (* aScale)
            else Left $ show a <> " cannot be converted to " <> show b

castMaybeUnit :: (Show a, UnitLike a, Show b, UnitLike b) => Maybe a -> Maybe b -> Either String (CReal -> CReal)
castMaybeUnit = unitMaybeify id castUnit

scaleUnit :: (UnitLike a, UnitLike b) => a -> b -> CReal -> CReal
scaleUnit a b x = x * toScale a / toScale b

scaleMaybeUnit :: (UnitLike a, UnitLike b) => Maybe a -> Maybe b -> CReal -> CReal
scaleMaybeUnit (Just a) (Just b) = scaleUnit a b
scaleMaybeUnit _ _ = id
