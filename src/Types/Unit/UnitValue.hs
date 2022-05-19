module Types.Unit.UnitValue where

import Types.Unit.Unit
import Data.Number.CReal
import Data.List
import Data.Bifunctor

data UnitValue = ContextUnit Unit | AdHocUnit [(Unit, CReal)]

reduceUnitValue :: UnitValue -> (CReal, [(String, CReal)])
reduceUnitValue (AdHocUnit us) = (1, us)
reduceUnitValue (ContextUnit u) = reduceUnit u

instance Show UnitValue where
    show (ContextUnit unit) = show unit
    show (AdHocUnit unitPowers) =
        let showPowerTuple (unit, power) =
                case power of
                    0 -> ""
                    1 -> unit
                    _ -> unit <> "^" <> show power

            multiplicationChain mapper predicate =
                intercalate "*"
                . fmap (showPowerTuple . second mapper)
                . filter (predicate . snd)
                $ first show <$> unitPowers

            positiveChain = multiplicationChain id (> 0)
            negativeChain = multiplicationChain negate (< 0)
            in case (null positiveChain, null negativeChain) of
                (_, True) -> positiveChain
                (True, _) -> "1/" <> negativeChain
                (_, _)    -> positiveChain <> "/" <> negativeChain


