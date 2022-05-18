module Types.Unit.Context where

import Control.Applicative
import Data.Foldable
import Data.List
import qualified Data.Map as DM
import Data.Number.CReal
import qualified Types.BiMap.BiMap as BM
import Types.Graph.Graph
import Types.Unit.Unit
import Utils.Map
import Data.Maybe

newtype UnitContext = UnitContext
    { unitsByBase :: DM.Map [(String, CReal)] (BM.BiMap CReal Unit)
    }

mkContext :: Foldable f => f Unit -> UnitContext
mkContext foldable =
    let groups = groupMap (sort . snd . reduceUnit) foldable
        unitsToBiMap :: [Unit] -> BM.BiMap CReal Unit
        unitsToBiMap =
            foldr
                ( \(unit, (quantity, _)) bm ->
                    BM.insert quantity unit bm
                )
                BM.empty
                . fmap (liftA2 (,) id reduceUnit)
     in UnitContext $ unitsToBiMap <$> groups

adjustUnit :: CReal -> Unit -> UnitContext -> Unit
adjustUnit q u ctx =
    let adjustedUnit = do
            let baseUnits = sort . snd . reduceUnit $ u
            unitClass <- DM.lookup baseUnits (unitsByBase ctx)
            unitQuantity <- BM.lookupB u unitClass
            let sameClassUnits = BM.pairs unitClass
            let candidateUnits = filter (\(xq, xu) -> q * xq / unitQuantity > 1) sameClassUnits
            if null candidateUnits then Nothing else Just . snd $ minimum candidateUnits
    in fromMaybe u adjustedUnit

times :: (CReal, Unit) -> (CReal, Unit) -> UnitContext -> (CReal, Unit)
times = undefined
