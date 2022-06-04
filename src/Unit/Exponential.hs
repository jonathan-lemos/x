{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
module Unit.Exponential where

import Data.Number.CReal
import qualified Data.Map as DM
import Data.Foldable
import Data.List

data Exponential a = Exponential { expBase :: a, expPower :: CReal }
    deriving (Eq, Ord)

modifyExpBase :: (a -> b) -> Exponential a -> Exponential b
modifyExpBase f e = e { expBase = f $ expBase e }

modifyExpPower :: (CReal -> CReal) -> Exponential a -> Exponential a
modifyExpPower f e = e { expPower = f $ expPower e }

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

showProduct :: (Show a) => [Exponential a] -> String
showProduct exps =
    let filterByPower p = filter (p . expPower) exps
        positive = show <$> filterByPower (> 0)
        negative = show . modifyExpPower negate <$> filterByPower (< 0)
        in intercalate "*" positive <> "/" <> intercalate "*" negative


instance {-# OVERLAPPING #-} Show (Exponential String) where
    show (Exponential b e) =
        case e of
            0 -> ""
            1 -> b
            n -> b <> "^" <> show n

instance Show a => Show (Exponential a) where
    show (Exponential b e) =
        case e of
            0 -> ""
            1 -> show b
            n -> show b <> "^" <> show n
