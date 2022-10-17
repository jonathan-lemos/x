{-# OPTIONS_GHC -Wno-unused-matches #-}

module X.Data.LeftAssociativeInfixList where

import Data.Bifunctor
import Data.List
import X.Utils.LeftToRight
import Prelude hiding (foldl1, foldr1)
import X.Data.Display

data LeftAssociativeInfixList op val = LeftAssociativeInfixList op val :<: (op, val) | InfixLeaf val
    deriving (Eq, Ord)

__stringifyLAIL :: (op -> String) -> (val -> String) -> LeftAssociativeInfixList op val -> String
__stringifyLAIL showOp showVal l =
        let (x, xs) = toHeadAndList l
         in xs
                |@>| (\(a, b) -> showOp a <> showVal b)
                @> intercalate ""
                @> (showVal x <>)

instance (Show op, Show val) => Show (LeftAssociativeInfixList op val) where
    show = __stringifyLAIL show show

instance (Display op, Display val) => Display (LeftAssociativeInfixList op val) where
    display = __stringifyLAIL display display

instance Functor (LeftAssociativeInfixList op) where
    fmap f (InfixLeaf v) = InfixLeaf (f v)
    fmap f (tree :<: (op, val)) = fmap f tree :<: (op, f val)

instance Bifunctor LeftAssociativeInfixList where
    bimap _a b (InfixLeaf v) = InfixLeaf (b v)
    bimap a b (tree :<: (op, val)) = bimap a b tree :<: (a op, b val)

toHeadAndList :: LeftAssociativeInfixList op val -> (val, [(op, val)])
toHeadAndList (InfixLeaf v) = (v, [])
toHeadAndList (tree :<: (op, val)) = toHeadAndList tree @> second (++ [(op, val)])

toList :: op -> LeftAssociativeInfixList op val -> [(op, val)]
toList initialOp l =
    let (head, xs) = toHeadAndList l
     in (initialOp, head):xs

toValueList :: LeftAssociativeInfixList op val -> [val]
toValueList l =
    let (head, xs) = toHeadAndList l
     in head:(xs |@>| snd)

fold :: (val -> (op, val) -> val) -> LeftAssociativeInfixList op val -> val
fold f (InfixLeaf l) = l
fold f (tree :<: (op, val)) = f (fold f tree) (op, val)
