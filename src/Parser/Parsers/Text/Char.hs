module Parser.Parsers.Text.Char where

import Parser.Parser

-- | A Parser that reads a single character from the input, failing if EOF is reached.
--
-- ## __Examples__
--
-- >>> parse char "abc"
-- Just ("bc",'a')
--
-- >>> parse char ""
-- Nothing
char :: Parser Char
char =
    Parser
        { name = "character"
        , expected = ["any character"]
        , parse = f
        }
  where
    f (x : xs) = Just (xs, x)
    f [] = Nothing
