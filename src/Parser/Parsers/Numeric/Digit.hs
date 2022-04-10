module Parser.Parsers.Numeric.Digit where

import Parser.Parser
import Data.Char
import Parser.Parsers.Conditional
import Parser.Parsers.Char

digit :: Parser Int
digit = read . (:[]) <$> conditional char isDigit
