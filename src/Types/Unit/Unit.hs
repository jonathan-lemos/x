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
    deriving (Ord)

renameUnit :: String -> Unit -> Unit
renameUnit s (BaseUnit b) = BaseUnit s
renameUnit s du = du{duName = s}

{- | Reduces a unit to its base units and powers.
For example, a kN would become (1000, [("kg", 1), ("m", 1), ("s", -2)])
-}
reduceUnit :: Unit -> (CReal, [(String, CReal)])
reduceUnit u =
    let _reduceBaseUnit :: String -> (CReal, DM.Map String CReal)
        _reduceBaseUnit name = (1, DM.singleton name 1)

        _reduceDerivedUnit :: CReal -> [(Unit, CReal)] -> (CReal, DM.Map String CReal)
        _reduceDerivedUnit quant subunits =
            let reducedSubunits = first _reduceUnitMap <$> subunits
                newQuantity = product (fst . fst <$> reducedSubunits)
                newUnits =
                    foldr
                        ( \((_, thisUnits), exponent) totalUnits ->
                            let adjustedUnits = fmap (* exponent) thisUnits
                             in DM.unionWith (+) totalUnits adjustedUnits
                        )
                        DM.empty
                        reducedSubunits
             in (newQuantity, newUnits)

        _reduceUnitMap :: Unit -> (CReal, DM.Map String CReal)
        _reduceUnitMap unit = case unit of
            BaseUnit name -> _reduceBaseUnit name
            DerivedUnit {duQuantity=q, duUnitPowers=up} -> _reduceDerivedUnit q up

     in second (filter ((/= 0) . snd) . DM.assocs) $ _reduceUnitMap u

{- | Divides the first base units by the second

## Examples

>>> [("kg", 2),("m", 2),("s",-1)] `baseUnitDiv` [("k",2),("s",-1),("m",3)]
[("k",-2),("kg",2),("m",-1)]
-}
baseUnitDiv :: [(String, CReal)] -> [(String, CReal)] -> [(String, CReal)]
baseUnitDiv a b = DM.assocs $ DM.unionWith (+) (DM.fromList a) (negate <$> DM.fromList b)

instance Eq Unit where
    a == b =
        let stableReduceUnit = second (sort . filter ((/= 0) . snd)) . reduceUnit
         in stableReduceUnit a == stableReduceUnit b

instance Show Unit where
    show (BaseUnit s) = s
    show (DerivedUnit s _ _) = s
