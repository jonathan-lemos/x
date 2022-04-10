module Parser.Parsers.Whitespace where

import Parser.Parser
import Data.Char
import Parser.Parsers.Chars

whitespace :: Parser String 
whitespace = chars isSpace
