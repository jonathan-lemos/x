module X.Control.Parser.AST.Assignment where

import X.Data.AST.Assignment
import X.Control.Parser
import X.Control.Parser.AST.Token.Identifier
import X.Control.Parser.Text.Whitespace
import X.Control.Parser.Text.CharEq
import X.Control.Parser.AST.ArithmeticExpression
import X.Utils.LeftToRight

assignment :: Parser Assignment
assignment = do
    whitespace
    ident <- identifier
    whitespace
    charEq '='
    whitespace
    additiveExpression |@>| Assignment ident

