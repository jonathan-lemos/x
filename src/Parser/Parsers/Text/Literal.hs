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
 Just (" world","hello")

 >>> parse (literal "abc") "ab c"
 Nothing

 >>> parse (literal "abc") ""
 Nothing

 >>> parse (literal "") "abc"
 Just ("abc","")

 >>> parse (literal "") ""
 Just ("","")
-}
literal :: String -> Parser String
literal s = mconcat (fmap (fmap (: []) . charEq) s) <|> fail ("Expected " <> quote s)
