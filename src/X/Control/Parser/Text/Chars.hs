module X.Control.Parser.Text.Chars where

import Control.Applicative
import X.Control.Parser
import X.Control.Parser.Combinator.Branch.Conditional
import X.Control.Parser.Text.Char

-- | Reads zero or more characters that match the given `predicate`.
chars :: (Char -> Bool) -> Parser String
chars predicate = many $ conditional predicate char
