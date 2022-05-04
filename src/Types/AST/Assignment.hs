module Types.AST.Assignment where

import Types.AST.ArithmeticExpression

data Assignment = Assignment { to :: String, value :: ArithmeticExpression }
