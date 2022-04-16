module Parser.Parsers.Text.CharEq where

import Parser.Parser
import Parser.Parsers.Text.CharAny

charEq :: Char -> Parser Char
charEq = charAny . (:[])
