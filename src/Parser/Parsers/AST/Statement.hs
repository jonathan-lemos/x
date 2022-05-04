module Parser.Parsers.AST.Statement (statement) where

import Types.AST.Statement

statement :: Parser Statement
statement = do
    
