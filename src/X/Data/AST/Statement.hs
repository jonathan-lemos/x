module X.Data.AST.Statement where

import X.Data.AST.Assignment
import X.Data.Value

-- | A top-level statement that the interpreter can execute.
data Statement = StmtValue Value | StmtAssignment Assignment
    deriving Eq

instance Show Statement where
    show (StmtValue val) = show val
    show (StmtAssignment assgn) = show assgn
