module Utils.Map where

import Data.Foldable
import qualified Data.Map as DM

count :: (Ord o, Foldable f, Num n) => f o -> DM.Map o n
count = foldl' (\a c -> DM.insertWith (+) c 1 a) DM.empty

groupMap :: (Ord k, Foldable f) => (v -> k) -> f v -> DM.Map k [v]
groupMap f = foldl' (\a c -> DM.adjust (c :) (f c) . DM.insertWith (const id) (f c) [] $ a) DM.empty
