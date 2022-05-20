module Types.Unit.ValueUnit where

import Types.Unit.ContextUnit
import Types.Unit.Exponential

data ValueUnit = ValueContextUnit ContextUnit | AdHocUnit [Exponential ValueUnit]
    deriving Ord

toContextUnits :: ValueUnit -> [Exponential ContextUnit]
toContextUnits (ValueContextUnit u) = [Exponential u 1]
toContextUnits (AdHocUnit us) = reduceExponentials . concat $ (\(Exponential b e) -> toContextUnits b) <$> us

instance Show ValueUnit where
    show (ValueContextUnit vcu) = show vcu
    show (AdHocUnit us) = undefined

instance Eq ValueUnit where
    a == b = toContextUnits a == toContextUnits b
