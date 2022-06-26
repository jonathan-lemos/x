module X.Data.AST.Assignment where

import X.Data.AST.ArithmeticExpression

data Assignment = Assignment { to :: String, value :: ArithmeticExpression }
    deriving (Eq, Show)
