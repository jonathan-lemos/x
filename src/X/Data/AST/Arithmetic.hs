module X.Data.AST.Arithmetic where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Number.CReal
import X.Data.Context
import X.Data.LeftAssociativeInfixList
import X.Data.Operator

newtype AdditiveChain = AdditiveChain (LeftAssociativeInfixList AdditiveOperator MultiplicativeChain)
    deriving (Eq, Ord, Show)

instance Display AdditiveChain where
    display (AdditiveChain l) = display l

newtype MultiplicativeChain = MultiplicativeChain (LeftAssociativeInfixList MultiplicativeOperator Factor)
    deriving (Eq, Ord, Show)

instance Display MultiplicativeChain where
    display (MultiplicativeChain l) = display l

newtype ExpChain = Exponentiation (NonEmpty Factor)
    deriving (Eq, Ord, Show)

instance Display ExpChain where
    display l =
        l |@>| display |@>| NE.intersperse "^" |@>| join

data Factor = Scalar CReal | Variable String | Parentheses AdditiveChain
    deriving (Eq, Ord, Show)
