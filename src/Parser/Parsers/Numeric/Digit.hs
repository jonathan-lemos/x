module Parser.Parsers.Numeric.Digit where

import Parser.Parser
import Data.Char
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char

digit :: Parser Int
digit = read . (:[]) <$> conditional char isDigit
