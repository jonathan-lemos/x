module X.Utils.Map where

import Data.Foldable
import qualified Data.Map as DM
import X.Utils.LeftToRight

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

adjustWithDefault :: (Ord k) => (v -> v) -> v -> k -> DM.Map k v -> DM.Map k v
adjustWithDefault mapper ifNotPresent key =
    DM.insertWith (const id) key ifNotPresent |@>| DM.adjust mapper key
