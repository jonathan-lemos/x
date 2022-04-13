module Parser.Parsers.Text.CharEq where

import Parser.Parser
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char

charEq :: Char -> Parser Char
charEq c = conditional char (== c)
