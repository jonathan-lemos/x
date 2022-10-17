module X.Data.AST.Arithmetic where

import Control.Monad
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Number.CReal
import X.Data.Display
import X.Data.LeftAssociativeInfixList
import X.Data.Operator
import X.Utils.LeftToRight

newtype AdditiveChain = AdditiveChain (LeftAssociativeInfixList AdditiveOperator MultiplicativeChain)
    deriving (Eq, Ord, Show)

instance Display AdditiveChain where
    display (AdditiveChain l) = display l

newtype MultiplicativeChain = MultiplicativeChain (LeftAssociativeInfixList MultiplicativeOperator ExpChain)
    deriving (Eq, Ord, Show)

instance Display MultiplicativeChain where
    display (MultiplicativeChain l) = display l

newtype ExpChain = Exponentiation (NonEmpty Factor)
    deriving (Eq, Ord, Show)

instance Display ExpChain where
    display (Exponentiation l) =
        l
        |@>| display
        @> NE.intersperse "^"
        @> NE.toList
        @> join

data Factor = Scalar CReal | Variable String | Parentheses AdditiveChain
    deriving (Eq, Ord, Show)

instance Display Factor where
    display (Scalar sc) = show sc
    display (Variable x) = show x
    display (Parentheses ac) = "(" <> display ac <> ")"
