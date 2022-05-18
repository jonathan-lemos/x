module Types.BiMap.BiMap where

import qualified Data.Map as DM

data BiMap a b = BiMap {
    aToB :: DM.Map a b,
    bToA :: DM.Map b a
}

empty :: BiMap a b
empty = BiMap DM.empty DM.empty

insert :: (Ord a, Ord b) => a -> b -> BiMap a b -> BiMap a b
insert na nb (BiMap a b) = BiMap (DM.insert na nb a) (DM.insert nb na b)

lookupA :: (Ord a, Ord b) => a -> BiMap a b -> Maybe b
lookupA a = DM.lookup a . aToB

lookupB :: (Ord a, Ord b) => b -> BiMap a b -> Maybe a
lookupB b = DM.lookup b . bToA

pairs :: (Ord a, Ord b) => BiMap a b -> [(a, b)]
pairs = DM.assocs . aToB
