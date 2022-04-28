module Parser.Parsers.Text.CharAny where

import Control.Applicative
import Data.List
import Parser.Parser
import Parser.Parsers.Combinator.Check
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char
import Utils.String (quoteShow)

{- | Reads a character that matches any of the characters in the given string, failing on EOF or a character not in the string

 ## __Examples__

 >>> parse (charAny "ab") "abc"
 Just ("bc",'a')

 >>> parse (charAny "bc") "abc"
 Nothing

 >>> parse (charAny "") "abc"
 Nothing

 >>> parse (charAny "ab") ""
 Nothing
-}
charAny :: String -> Parser Char
charAny s =
    check
        (`elem` s)
        (const ("Expected any of " <> intercalate ", " (fmap quoteShow s)))
        char
