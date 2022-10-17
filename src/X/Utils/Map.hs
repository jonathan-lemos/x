module X.Utils.Map where

import Data.Foldable
import qualified Data.Map as DM
import X.Utils.LeftToRight

insertIfNotExists :: Ord k => k -> v -> DM.Map k v -> DM.Map k v
insertIfNotExists = DM.insertWith const

adjustWithDefault :: Ord k => k -> (v -> v) -> v -> DM.Map k v -> DM.Map k v
adjustWithDefault k f v m =
    insertIfNotExists k v m
    @> DM.adjust f k

count :: (Ord o, Foldable f, Num n) => f o -> DM.Map o n
count = foldl' (\a c -> DM.insertWith (+) c 1 a) DM.empty

groupMap :: (Ord k, Foldable f) => (a -> k) -> (a -> v) -> f a -> DM.Map k [v]
groupMap key value =
    foldr
        ( \x ->
            DM.insertWith (\_ b -> b) (key x) []
                |@>| DM.adjust (value x :) (key x)
        )
        DM.empty
