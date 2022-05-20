module Types.Unit.Exponential where

import Data.Number.CReal
import qualified Data.Map as DM
import Data.Foldable
import Data.Bifunctor

data Exponential a = Exponential { expBase :: a, expPower :: CReal }
    deriving (Eq, Ord)

modifyExpBase :: (a -> b) -> Exponential a -> Exponential b
modifyExpBase f exp = exp { expBase = f $ expBase exp }

modifyExpPower :: (CReal -> CReal) -> Exponential a -> Exponential a
modifyExpPower f exp = exp { expPower = f $ expPower exp }

_exponentialMap :: (Ord a) => [Exponential a] -> DM.Map a CReal
_exponentialMap a =
    let addExp (Exponential b e) = DM.insertWith (+) b e
        in foldl' (flip addExp) DM.empty a

reduceExpProduct :: (Ord a) => [Exponential a] -> [Exponential a]
reduceExpProduct a = fmap (uncurry Exponential) . DM.assocs $ _exponentialMap a

mergeExpProducts :: (Ord a) => [Exponential a] -> [Exponential a] -> [Exponential a]
mergeExpProducts a b =
    fmap (uncurry Exponential) . DM.assocs $ DM.unionWith (+) (_exponentialMap a) (_exponentialMap b)

expComplement :: [Exponential a] -> [Exponential a]
expComplement = fmap $ modifyExpPower negate

expProductDifference :: (Ord a) => [Exponential a] -> [Exponential a] -> [Exponential a]
expProductDifference a b = mergeExpProducts a (expComplement b)


instance Show a => Show (Exponential a) where
    show (Exponential base exp) =
        case exp of
            0 -> ""
            1 -> show base
            _ -> show base <> "^"
