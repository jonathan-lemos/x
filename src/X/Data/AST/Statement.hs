module X.Data.AST.Statement where

import X.Data.AST.ArithmeticExpression
import X.Data.AST.Assignment

-- | A top-level statement that the interpreter can executer.
data Statement = StmtExpr ArithmeticExpression | StmtAssignment Assignment
    deriving (Eq, Show)
