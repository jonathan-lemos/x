module Parser.Parsers.Text.CharAny where

import Control.Applicative
import Parser.Parser
import Parser.Parsers.Combinator.Branch.Check
import Parser.Parsers.Text.Char
import Utils.String (pluralize)
import Parser.Parsers.Combinator.Atomic

{- | Reads a character that matches any of the characters in the given string, failing on EOF or a character not in the string

 ## __Examples__

 >>> parse (charAny "ab") "abc"
 Right ("bc",'a')

 >>> parse (charAny "bc") "abc"
 Left (ParseError {reason = "Expected 'b' or 'c'", currentInput = "abc"})

 >>> parse (charAny "ab") ""
 Left (ParseError {reason = "Expected 'a' or 'b'", currentInput = ""})
-}
charAny :: String -> Parser Char
charAny s =
    let errMsg = "Expected " <> pluralize (fmap show s)
    in atomic $ check
        (`elem` s)
        (const errMsg)
        (char <|> fail errMsg)
