module Parser.Parsers.AST.Statement where

import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parsers.AST.Token.Identifier
import Parser.Parsers.Combinator.FirstThatParses
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Text.Whitespace
import Types.AST.Assignment
import Types.AST.Statement
import Parser.Parsers.Combinator.Choice.LookaheadParse
import Parser.Parsers.AST.Assignment

statement :: Parser Statement
statement =
    whitespace
        >> lookaheadParse [
            identifier >> whitespace >> charEq '=' >> pure (StmtAssignment <$> assignment),
            pure (StmtExpr <$> arithmeticExpression)
        ]
