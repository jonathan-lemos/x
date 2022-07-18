module X.Data.Context where

import qualified Data.Map as DM
import X.Data.Value
import X.Utils.Function
import Data.Foldable

type Context = [DM.Map String Value]

new :: Context
new = [DM.empty]

put :: String -> Value -> Context -> Context
put key value (x:xs) = DM.insert key value x : xs
put _ _ [] = []

get :: String -> Context -> Maybe Value
get key = fmap (DM.lookup key) |> asum

push :: Context -> Context
push = (DM.empty :)

pop :: Context -> Context
pop = tail
