module Types.AST.Statement where
import Types.AST.ArithmeticExpression
import Types.AST.Assignment

data Statement = StmtExpr ArithmeticExpression | StmtAssignment Assignment
    deriving (Eq, Show)
