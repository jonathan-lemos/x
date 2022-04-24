module Parser.Parsers.Text.CharEq where

import Parser.Parser
import Parser.Parsers.Text.CharAny

-- | Reads a character equal to the given character, failing on EOF or on a different character
--
-- ## __Examples__
--
-- >>> parse (charEq 'a') "abc"
-- Just ("bc",'a')
--
-- >>> parse (charEq 'a') "bc"
-- Nothing
--
-- >>> parse (charEq 'a') ""
-- Nothing
charEq :: Char -> Parser Char
charEq c =
    (charAny [c]) {
        name = [c],
        expected = [[c]]
    }
