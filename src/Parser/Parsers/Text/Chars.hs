module Parser.Parsers.Text.Chars where

import Control.Applicative
import Parser.Parser
import Parser.Parsers.Combinator.Branch.Conditional
import Parser.Parsers.Text.Char

{- | Reads zero or more characters that match the given `predicate`.

 ## __Examples__

 >>> import Data.Char
 >>> parse (chars isAlpha) "hello world"
 Right (" world","hello")

 >>> parse (chars $ const False) "hello world"
 Right ("hello world","")

 >>> parse (chars $ const True) ""
 Right ("","")
-}
chars :: (Char -> Bool) -> Parser String
chars predicate = many $ conditional predicate char
