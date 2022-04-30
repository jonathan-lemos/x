module Parser.Parsers.Text.Whitespace where

import Data.Char
import Parser.Parser
import Parser.Parsers.Text.Chars

{- | Matches zero or more whitespace characters.

 ## __Examples__

 >>> parse whitespace "   foo bar"
 Right ("foo bar","   ")

 >>> parse whitespace "foo bar"
 Right ("foo bar","")

 >>> parse whitespace ""
 Right ("","")
-}
whitespace :: Parser String
whitespace = chars isSpace
