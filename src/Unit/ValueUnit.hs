module Unit.ValueUnit where

import Unit.ContextUnit
import Unit.Exponential

data ValueUnit = ValueContextUnit ContextUnit | AdHocUnit [Exponential ValueUnit]
    deriving Ord

toContextUnits :: ValueUnit -> [Exponential ContextUnit]
toContextUnits (ValueContextUnit u) = [Exponential u 1]
toContextUnits (AdHocUnit us) = reduceExpProduct . concat $ (\(Exponential b _e) -> toContextUnits b) <$> us

instance Show ValueUnit where
    show (ValueContextUnit vcu) = show vcu
    show (AdHocUnit us) = showProduct us

instance Eq ValueUnit where
    a == b = toContextUnits a == toContextUnits b
