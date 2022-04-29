module Parser.Parsers.Text.Literal where

import Control.Applicative
import Parser.Parser
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char
import Parser.Parsers.Text.CharEq
import Utils.String

{- | Matches the given string, failing if the immediate input doesn't equal it.

 ## __Examples__

 >>> parse (literal "hello") "hello world"
 Right (" world","hello")

 >>> parse (literal "abc") "ab c"
 Left (ParseError {reason = "Expected \"abc\"", currentInput = "ab c"})

 >>> parse (literal "abc") ""
 Left (ParseError {reason = "Expected \"abc\"", currentInput = ""})

 >>> parse (literal "") "abc"
 Right ("abc","")

 >>> parse (literal "") ""
 Right ("","")
-}
literal :: String -> Parser String
literal s = mconcat (fmap (fmap (: []) . charEq) s) <|> fail ("Expected " <> show s)
