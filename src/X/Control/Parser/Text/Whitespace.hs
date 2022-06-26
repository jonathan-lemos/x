module X.Control.Parser.Text.Whitespace where

import Data.Char
import X.Control.Parser
import X.Control.Parser.Text.Chars

-- | Matches zero or more whitespace characters.
whitespace :: Parser String
whitespace = chars isSpace
