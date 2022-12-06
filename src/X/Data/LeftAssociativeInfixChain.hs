module X.Data.LeftAssociativeInfixChain where

import Data.Bifunctor
import X.Utils.LeftToRight
import X.Data.Operator

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

toList :: (ChainHeadOp op) => LeftAssociativeInfixChain op val -> [(op, val)]
toList = toListWithInitialOperator chainHeadOp

toListWithInitialOperator :: op -> LeftAssociativeInfixChain op val -> [(op, val)]
toListWithInitialOperator op list =
    let (head, tail) = toHeadAndList list
     in (op, head) : tail

stringify :: (op -> String) -> (val -> String) -> LeftAssociativeInfixChain op val -> String
stringify _ g (Leaf val) = g val
stringify f g (Link sublist op val) = stringify f g sublist <> f op <> g val

concat :: LeftAssociativeInfixChain op val -> op -> LeftAssociativeInfixChain op val -> LeftAssociativeInfixChain op val
concat a op b =
    let (ah, at) = toHeadAndList a
        (bh, bt) = toHeadAndList b
     in fromHeadAndList ah (at <> ((op, bh) : bt))

append :: LeftAssociativeInfixChain op val -> op -> val -> LeftAssociativeInfixChain op val
append (Leaf lval) op val = Link (Leaf lval) op val
append (Link sublist lop lval) op val = Link (Link sublist lop lval) op val

prepend :: val -> op -> LeftAssociativeInfixChain op val -> LeftAssociativeInfixChain op val
prepend lval lop (Leaf rval) = Link (Leaf lval) lop rval
prepend lval lop (Link sublist rop rval) = Link (prepend lval lop sublist) rop rval

