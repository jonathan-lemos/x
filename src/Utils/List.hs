module Utils.List where

import Data.Bifunctor

indexes :: Foldable f => [f a] -> [(Int, f a)]
indexes =
    let go count (x:xs) = (count, x) : go (count + length x) xs
        go _ [] = []
    in go 0

lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse
