module Parser.Parsers.Text.CharAny where

import Parser.Parser
import Control.Applicative
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char

charAny :: String -> Parser Char
charAny s = conditional char (`elem` s)
