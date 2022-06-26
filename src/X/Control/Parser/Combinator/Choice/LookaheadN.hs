module X.Control.Parser.Combinator.Choice.LookaheadN where

import X.Control.Parser.Combinator.Choice.Lookahead
import X.Control.Parser

-- | Looks at the remaining input (up to N characters) and chooses a Parser based on it
lookaheadN :: Int -> (String -> Parser a) -> Parser a
lookaheadN n f = lookahead (f . take n)


