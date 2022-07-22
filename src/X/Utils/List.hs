module X.Utils.List(indexes, lastN, sortByKey) where
import Control.Applicative
import X.Utils.LeftToRight
import Data.List

indexes :: Foldable f => [f a] -> [(Int, f a)]
indexes =
    let go count (x:xs) = (count, x) : go (count + length x) xs
        go _ [] = []
    in go 0

lastN :: Int -> [a] -> [a]
lastN n = reverse . take n . reverse

data SortDTO v k = SortDTO v k

instance Eq k => Eq (SortDTO v k) where
    SortDTO _ k == SortDTO _ k2 = k == k2

instance Ord k => Ord (SortDTO v k) where
    compare (SortDTO _ k1) (SortDTO _ k2) = compare k1 k2

-- | Sorts the list based on the values that the given function produces. This sort is stable.
sortByKey :: (Ord k) => (a -> k) -> [a] -> [a]
sortByKey f =
    fmap (liftA2 SortDTO id f)
    |@>| sort
    ||@>|| \(SortDTO v _) -> v
