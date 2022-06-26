module X.Control.Parser.Text.Char where

import X.Data.ParseError
import X.Control.Parser

-- | Parses a single character, failing if EOF is reached.
char :: Parser Char
char =
    let f (x : xs) = Right (xs, x)
        f [] = Left $ ParseError "Expected any character" ""
     in Parser f
