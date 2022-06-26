{-# LANGUAGE FlexibleInstances #-}
module X.Data.Unit.Unit where

import Data.Number.CReal
import X.Data.Unit.UnitLike
import X.Data.Unit.UnitScaleOperation
import X.Utils.String
import X.Data.Unit.Exponential
import X.Utils.CReal

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

instance {-# OVERLAPPING #-} Show (Maybe Unit) where
    show (Just u) = show u
    show Nothing = "unitless quantity"

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

downscale :: Unit -> CReal -> CReal
downscale (BaseUnit _) = id
downscale (ScaledUnit _name uso base) =
    let apUso (Add n)          = (+ n) -- 0 C -> 273.15 K
        apUso (Multiply n)     = (* n) -- 1 kg -> 1000 g
        apUso (Exponentiate n) = (`absExp` (1 `safeDiv` n)) -- 1 km^2 -> 1000000 m^2
    in downscale base . apUso uso
downscale (ProductUnit _name ps) = (*) . product $ toScale <$> ps

upscale :: Unit -> CReal -> CReal
upscale (BaseUnit _) = id
upscale (ScaledUnit _name uso base) =
    let apUso (Add n)          = (+ negate n)  -- 0 K -> -273.15 C
        apUso (Multiply n)     = (`safeDiv` n) -- 1000 g -> 1 kg
        apUso (Exponentiate n) = (`absExp` n)  -- 1000000 m^2 -> 1 km^2
    in apUso uso . upscale base
upscale (ProductUnit _name ps) = (*) . (1 /) . product $ toScale <$> ps

castUnit :: Unit -> Unit -> Either String (CReal -> CReal)
castUnit a b =
    if toBaseUnits a == toBaseUnits b
        then Right $ upscale b . downscale a
        else Left $ show a <> " cannot be converted to " <> show b

castMaybeUnit :: Maybe Unit -> Maybe Unit -> Either String (CReal -> CReal)
castMaybeUnit = unitMaybeify id castUnit

scaleUnit :: (UnitLike a, UnitLike b) => a -> b -> CReal -> CReal
scaleUnit a b x = x * toScale a / toScale b

scaleMaybeUnit :: (UnitLike a, UnitLike b) => Maybe a -> Maybe b -> CReal -> CReal
scaleMaybeUnit (Just a) (Just b) = scaleUnit a b
scaleMaybeUnit _ _ = id
