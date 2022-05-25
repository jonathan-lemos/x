module Parser.Parsers.AST.Assignment where

import Types.AST.Assignment
import Parser.Parser
import Parser.Parsers.AST.Token.Identifier
import Parser.Parsers.Text.Whitespace
import Parser.Parsers.Text.CharEq
import Parser.Parsers.AST.ArithmeticExpression

assignment :: Parser Assignment
assignment = do
    whitespace
    ident <- identifier
    whitespace
    charEq '='
    whitespace
    Assignment ident <$> arithmeticExpression

