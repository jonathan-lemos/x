module Parser.Parsers.Text.Char where

import Parser.Error
import Parser.Parser

{- | Parses a single character, failing if EOF is reached.

 ## __Examples__

 >>> parse char "abc"
 Right ("bc",'a')

 >>> parse char ""
 Left (ParseError {reason = "Expected any character", currentInput = ""})
-}
char :: Parser Char
char =
    let f (x : xs) = Right (xs, x)
        f [] = Left $ ParseError "Expected any character" ""
     in Parser f
