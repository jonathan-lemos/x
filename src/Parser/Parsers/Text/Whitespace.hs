module Parser.Parsers.Text.Whitespace where

import Parser.Parser
import Data.Char
import Parser.Parsers.Text.Chars

-- | Matches zero or more whitespace characters.
--
-- ## __Examples__
--
-- >>> parse whitespace "   foo bar"
-- Right ("foo bar","   ")
--
-- >>> parse whitespace "foo bar"
-- Right ("foo bar","")
--
-- >>> parse whitespace ""
-- Right ("","")
whitespace :: Parser String
whitespace = chars isSpace
