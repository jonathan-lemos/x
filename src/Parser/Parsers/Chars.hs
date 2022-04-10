module Parser.Parsers.Chars where

import Parser.Parsers.Conditional
import Parser.Parsers.Char
import Parser.Parser
import Control.Applicative

chars :: (Char -> Bool) -> Parser String 
chars = many . conditional char
