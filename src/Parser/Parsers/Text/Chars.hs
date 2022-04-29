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
-- >>> parse (chars isAlpha) "hello world"
-- Right (" world","hello")
--
-- >>> parse (chars $ const False) "hello world"
-- Right ("hello world","")
--
-- >>> parse (chars $ const True) ""
-- Right ("","")
chars :: (Char -> Bool) -> Parser String
chars predicate = many $ conditional predicate char
