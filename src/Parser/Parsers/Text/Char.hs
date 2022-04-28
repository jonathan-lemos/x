module Parser.Parsers.Text.Char where

import Parser.Error
import Parser.Parser

{- | A Parser that reads a single character from the input, failing if EOF is reached.

 ## __Examples__

 >>> parse char "abc"
 Right ("bc",'a')

 >>> parse char ""
 Left (ParseError "Expected any character" "")
-}
char :: Parser Char
char =
    let f (x : xs) = Right (xs, x)
        f [] = Left $ ParseError "Expected any character" ""
     in Parser f
