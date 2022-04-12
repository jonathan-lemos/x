module Parser.Parsers.Text.Whitespace where

import Parser.Parser
import Data.Char
import Parser.Parsers.Text.Chars

whitespace :: Parser String 
whitespace = chars isSpace
