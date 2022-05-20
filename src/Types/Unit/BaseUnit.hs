module Types.Unit.BaseUnit where
import Data.Number.CReal
import Types.Unit.Exponential

class UnitClass u where
    toBaseUnitsAndQuantity :: u -> (CReal, [Exponential BaseUnit])

    toBaseUnits :: u -> [Exponential BaseUnit]
    toBaseUnits = snd . toBaseUnitsAndQuantity

newtype BaseUnit = BaseUnit String
    deriving (Eq, Ord)

instance Show BaseUnit where
    show (BaseUnit b) = show b

instance UnitClass BaseUnit where
    toBaseUnitsAndQuantity u = (1, [Exponential u 1])
