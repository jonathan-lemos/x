module X.Data.Context where

import qualified Data.Map as DM
import Data.Foldable
import X.Utils.LeftToRight

type Context = [DM.Map String ArithmeticUnion]

new :: Context
new = [DM.empty]

put :: String -> ArithmeticUnion -> Context -> Context
put key value (x:xs) = DM.insert key value x : xs
put _ _ [] = []

get :: String -> Context -> Maybe ArithmeticUnion
get key = fmap (DM.lookup key) |@>| asum

push :: Context -> Context
push = (DM.empty :)

pop :: Context -> Context
pop = tail
