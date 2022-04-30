module Parser.Parsers.Text.CharEq where

import Control.Applicative
import Parser.Parser
import Parser.Parsers.Text.CharAny
import Utils.String

{- | Reads a character equal to the given character, failing on EOF or on a different character

 ## __Examples__

 >>> parse (charEq 'a') "abc"
 Right ("bc",'a')

 >>> parse (charEq 'a') "bc"
 Left (ParseError {reason = "Expected 'a'", currentInput = "bc"})

 >>> parse (charEq 'a') ""
 Left (ParseError {reason = "Expected 'a'", currentInput = ""})
-}
charEq :: Char -> Parser Char
charEq c = charAny [c] <|> fail ("Expected " <> show c)
