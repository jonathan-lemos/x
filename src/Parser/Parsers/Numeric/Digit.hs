module Parser.Parsers.Numeric.Digit where

import Parser.Parser
import Data.Char
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char
import Control.Applicative

digit :: Parser Int
digit = read . (:[]) <$> conditional isDigit char <|> fail "Expected digit"
