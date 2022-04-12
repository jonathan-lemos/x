module Parser.Parsers.Text.Chars where

import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char
import Parser.Parser
import Control.Applicative

chars :: (Char -> Bool) -> Parser String 
chars = many . conditional char
