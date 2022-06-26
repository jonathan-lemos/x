module X.Control.Parser.Combinator.Choice.Lookahead where

import Control.Monad
import X.Control.Parser

-- | Looks at the remaining input and chooses a Parser based on it
lookahead :: (String -> Parser a) -> Parser a
lookahead f = join . Parser $ \s -> Right (s, f s)
