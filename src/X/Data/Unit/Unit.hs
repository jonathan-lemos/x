{-# LANGUAGE FlexibleInstances #-}

module X.Data.Unit.Unit where

import Data.Number.CReal
import X.Control.Try
import X.Data.Unit.Exponential
import X.Data.Unit.UnitLike
import X.Data.Unit.UnitScaleOperation
import X.Utils.CReal
import X.Utils.String

-- | Represents a unit, e.g. kg or m or kg*m/s^2
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

-- Shows two elements separated by an operator
_showInfixOp :: (Show a, Show b) => a -> String -> b -> String
_showInfixOp a op b = parenthesize $ show a <> op <> show b

{- | Takes a default and an infix function taking two units, and returns an equivalent infix function taking two (Maybe unit)s

 If both input values are Nothing, the default is returned
 If one Just is given but not the other, an error is returned indicating that a unitless quantity cannot be converted into the Just unit
 If two Justs are given, the result of the given infix function is passed through
-}
unitMaybeify :: (Show a, UnitLike a, Show b, UnitLike b) => c -> (a -> b -> Try c) -> Maybe a -> Maybe b -> Try c
unitMaybeify _def f (Just a) (Just b) = f a b
unitMaybeify def _f Nothing Nothing = Success def
unitMaybeify _def _f (Just a) Nothing = fail $ show a <> " cannot be converted to unitless quantity"
unitMaybeify _def _f Nothing (Just b) = fail $ "unitless quantity cannot be converted to " <> show b

{- | Given a Unit, returns a function scaling a quantity of that unit down to the same quantity of the base units

 e.g. downscale Fahrenheit == \x -> ((x - 32) * 5/9) + 273.15
-}
downscale :: Unit -> CReal -> CReal
downscale (BaseUnit _) = id
downscale (ScaledUnit _name uso base) =
    let apUso (Add n) = (+ n) -- 0 C -> 273.15 K
        apUso (Multiply n) = (* n) -- 1 kg -> 1000 g
        apUso (Exponentiate n) = (`absExp` (1 `safeDiv` n)) -- 1 km^2 -> 1000000 m^2
     in downscale base . apUso uso
downscale (ProductUnit _name ps) = (*) . product $ toScale <$> ps

{- | Given a Unit, returns a function scaling a quantity of the base unit up to the same quantity of the given unit

 e.g. upscale Fahrenheit == \x -> ((x - 273.15) * 9/5) + 32
-}
upscale :: Unit -> CReal -> CReal
upscale (BaseUnit _) = id
upscale (ScaledUnit _name uso base) =
    let apUso (Add n) = (+ negate n) -- 0 K -> -273.15 C
        apUso (Multiply n) = (`safeDiv` n) -- 1000 g -> 1 kg
        apUso (Exponentiate n) = (`absExp` n) -- 1000000 m^2 -> 1 km^2
     in apUso uso . upscale base
upscale (ProductUnit _name ps) = (*) . (1 /) . product $ toScale <$> ps

{- | Returns a function converting one unit quantity to another, or an error message indicating that the conversion cannot be done

 e.g. castUnit kg g == (* 1000)
      castUnit g kg == (/ 1000)
-}
castUnit :: Unit -> Unit -> Try (CReal -> CReal)
castUnit a b =
    if toBaseUnits a == toBaseUnits b
        then Success $ upscale b . downscale a
        else fail $ show a <> " cannot be converted to " <> show b

-- | castUnit for (Maybe Unit)s. If two Nothings are given, returns the identity function
castMaybeUnit :: Maybe Unit -> Maybe Unit -> Try (CReal -> CReal)
castMaybeUnit = unitMaybeify id castUnit

-- | castUnit ignoring any +'s or -'s. This function is necessary to make kg*C equal to kg*K.
scaleUnit :: (UnitLike a, UnitLike b) => a -> b -> CReal -> CReal
scaleUnit a b x = x * toScale a / toScale b

-- | castMaybeUnit ignoring any +'s or -'s. This function is necessary to make kg*C equal to kg*K.
scaleMaybeUnit :: (UnitLike a, UnitLike b) => Maybe a -> Maybe b -> CReal -> CReal
scaleMaybeUnit (Just a) (Just b) = scaleUnit a b
scaleMaybeUnit _ _ = id
