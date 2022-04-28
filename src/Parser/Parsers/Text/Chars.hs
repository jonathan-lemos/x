module Parser.Parsers.Text.Chars where

import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char
import Parser.Parser
import Control.Applicative

-- | Reads zero or more characters that match the given `predicate`, with the given `name` (which does not affect the behavior of the parser, only how it appears in error messages).
--
-- ## __Examples__
--
-- >>> import Data.Char
-- >>> parse (chars isAlpha "alpha characters") "hello world"
-- Just (" world","hello")
--
-- >>> parse (chars (const False) "never") "hello world"
-- Just ("hello world","")
--
-- >>> parse (chars (const True) "always") ""
-- Just ("","")
chars :: (Char -> Bool) -> Parser String
chars predicate = many $ conditional predicate char
