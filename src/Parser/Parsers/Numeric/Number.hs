module Parser.Parsers.Numeric.Number where

import Control.Applicative
import Parser.Parsers.Numeric.Digit
import Parser.Parser
import Parser.Parsers.Combinator.Conditional
import Data.Char
import Parser.Parsers.Text.Char
import Parser.Parsers.Text.CharEq

integer :: Parser Integer
integer = foldl (\a c -> a * 10 + toInteger c) 0 <$> some digit
