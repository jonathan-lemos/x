module X.Control.Parser.Text.CharEq where

import Control.Applicative
import X.Control.Parser
import X.Control.Parser.Text.CharAny

-- | Reads a character equal to the given character, failing on EOF or on a different character
charEq :: Char -> Parser Char
charEq c = charAny [c] <|> fail ("Expected " <> show c)
