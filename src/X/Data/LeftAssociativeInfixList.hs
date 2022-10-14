{-# OPTIONS_GHC -Wno-unused-matches #-}

module X.Data.LeftAssociativeInfixList where

import Data.Bifunctor
import Data.List
import X.Utils.LeftToRight
import Prelude hiding (foldl1, foldr1)

data LeftAssociativeInfixList op val = LeftAssociativeInfixList op val :<: (op, val) | InfixLeaf val
    deriving (Eq, Ord)

instance (Show op, Show val) => Show (LeftAssociativeInfixList op val) where
    show l =
        let (x, xs) = toHeadAndList l
         in xs
                |@>| (\(a, b) -> show a <> show b)
                @> intercalate ""
                @> (show x <>)

instance Functor (LeftAssociativeInfixList op) where
    fmap f (InfixLeaf v) = InfixLeaf (f v)
    fmap f (tree :<: (op, val)) = fmap f tree :<: (op, f val)

instance Bifunctor LeftAssociativeInfixList where
    bimap _a b (InfixLeaf v) = InfixLeaf (b v)
    bimap a b (tree :<: (op, val)) = bimap a b tree :<: (a op, b val)

toHeadAndList :: LeftAssociativeInfixList op val -> (val, [(op, val)])
toHeadAndList (InfixLeaf v) = (v, [])
toHeadAndList (tree :<: (op, val)) = toHeadAndList tree @> second (++ [(op, val)])

toValueList :: LeftAssociativeInfixList op val -> [val]
toValueList l =
    let (head, xs) = toHeadAndList l
     in head:(xs |@>| snd)

fold :: (val -> (op, val) -> val) -> LeftAssociativeInfixList op val -> val
fold f (InfixLeaf l) = l
fold f (tree :<: (op, val)) = f (fold f tree) (op, val)
