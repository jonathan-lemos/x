module Types.Unit.Unit where

import Control.Applicative
import Data.Bifunctor
import Data.List
import qualified Data.Map as DM
import Data.Number.CReal

data Unit
    = BaseUnit
        { buName :: String
        }
    | DerivedUnit
        { duName :: String
        , duQuantity :: CReal
        , duUnitPowers :: [(Unit, CReal)]
        }
    | AdHocUnit
        { ahUnitPowers :: [(Unit, CReal)]
        }
    deriving (Ord)

unitName :: Unit -> String
unitName (BaseUnit s) = s
unitName (DerivedUnit s _ _) = s

renameUnit :: String -> Unit -> Unit
renameUnit s (BaseUnit b) = BaseUnit s
renameUnit s du = du{derivedUnitName = s}

{- | Reduces a unit to its base units and powers.
For example, a kN would become (1000, [("kg", 1), ("m", 1), ("s", -2)])
-}
reduceUnit :: Unit -> (CReal, [(String, CReal)])
reduceUnit u =
    let _reduceUnitMap :: Unit -> (CReal, DM.Map String CReal)
        _reduceUnitMap (BaseUnit s) = (1, DM.singleton s 1)
        _reduceUnitMap (DerivedUnit _ q l) =
            let reduced = first _reduceUnitMap <$> l
                quantity = product (fst . fst <$> reduced)
                newUnits =
                    foldr
                        ( \((_, thisUnits), exponent) totalUnits ->
                            let adjustedUnits = fmap (* exponent) thisUnits
                             in DM.unionWith (+) totalUnits adjustedUnits
                        )
                        DM.empty
                        reduced
             in (quantity, newUnits)
     in second DM.assocs $ _reduceUnitMap u

unitMul :: Unit -> Unit -> Unit
unitMul a b =
    let name = unitName a <> "*" <> unitName b
        q (BaseUnit _) = 1
        q (DerivedUnit{quantity = quan}) = quan
     in DerivedUnit name (q a * q b) [(a, 1), (b, 1)]

unitDiv :: Unit -> Unit -> Unit
unitDiv a b =
    let name = unitName a <> "/" <> unitName b
        q (BaseUnit _) = 1
        q (DerivedUnit{quantity = quan}) = quan
     in DerivedUnit name (q a * q b) [(a, 1), (b, 1)]

{- | Divides the first base units by the second

## Examples

>>> [("kg", 2),("m", 2),("s",-1)] `baseUnitDiv` [("k",2),("s",-1),("m",3)]
[("k",-2),("kg",2),("m",-1)]
-}
baseUnitDiv :: [(String, CReal)] -> [(String, CReal)] -> [(String, CReal)]
baseUnitDiv a b = DM.assocs $ DM.unionWith (+) (DM.fromList a) (negate <$> DM.fromList b)

instance Eq Unit where
    a == b =
        let stableReduceUnit = second sort . reduceUnit
         in stableReduceUnit a == stableReduceUnit b

instance Show Unit where
    show (BaseUnit s) = s
    show (DerivedUnit s _ _) = s
