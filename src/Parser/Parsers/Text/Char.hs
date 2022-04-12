module Parser.Parsers.Text.Char where

import Parser.Parser

char :: Parser Char
char = Parser f where
    f (x:xs) = Just (xs, x)
    f [] = Nothing
