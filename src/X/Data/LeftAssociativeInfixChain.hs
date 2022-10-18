module X.Data.LeftAssociativeInfixChain where

import Data.Bifunctor
import X.Utils.LeftToRight

data LeftAssociativeInfixChain op val = Leaf val | Link (LeftAssociativeInfixChain op val) op val
    deriving (Eq, Ord, Show)

singleton :: val -> LeftAssociativeInfixChain op val
singleton = Leaf

instance Bifunctor LeftAssociativeInfixChain where
    bimap _ g (Leaf val) = Leaf (g val)
    bimap f g (Link sublist op val) = Link (bimap f g sublist) (f op) (g val)

instance Functor (LeftAssociativeInfixChain op) where
    fmap = second

fromHeadAndList :: val -> [(op, val)] -> LeftAssociativeInfixChain op val
fromHeadAndList v xs =
    let go v [] = Leaf v
        go v ((op, val) : xs) = Link (go v xs) op val
     in go v (reverse xs)

toHeadAndList :: LeftAssociativeInfixChain op val -> (val, [(op, val)])
toHeadAndList =
    let go (Leaf x) = (x, [])
        go (Link sublist op val) = go sublist |@>| ((op, val) :)
     in go |@>| second reverse

stringify :: (op -> String) -> (val -> String) -> (LeftAssociativeInfixChain op val) -> String
stringify _ g (Leaf val) = g val
stringify f g (Link sublist op val) = stringify f g sublist <> f op <> g val
