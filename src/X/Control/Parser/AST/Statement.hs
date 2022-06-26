module X.Control.Parser.AST.Statement where

import X.Control.Parser
import X.Control.Parser.AST.ArithmeticExpression
import X.Control.Parser.AST.Token.Identifier
import X.Control.Parser.Text.CharEq
import X.Control.Parser.Text.Whitespace
import X.Data.AST.Statement
import X.Control.Parser.Combinator.Choice.LookaheadParse
import X.Control.Parser.AST.Assignment

statement :: Parser Statement
statement =
    whitespace
        >> lookaheadParse [
            identifier >> whitespace >> charEq '=' >> pure (StmtAssignment <$> assignment),
            pure (StmtExpr <$> arithmeticExpression)
        ]
