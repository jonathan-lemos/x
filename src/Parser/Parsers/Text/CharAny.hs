module Parser.Parsers.Text.CharAny where

import Control.Applicative
import Parser.Parser
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char

-- | Reads a character that matches any of the characters in the given string, failing on EOF or a character not in the string
--
-- ## __Examples__
--
-- >>> parse (charAny "ab") "abc"
-- Just ("bc",'a')
--
-- >>> parse (charAny "bc") "abc"
-- Nothing
--
-- >>> parse (charAny "") "abc"
-- Nothing
--
-- >>> parse (charAny "ab") ""
-- Nothing
charAny :: String -> Parser Char
charAny s =
    (conditional char (`elem` s))
        { name = "any of " <> s
        , expected = fmap (: []) s
        }
