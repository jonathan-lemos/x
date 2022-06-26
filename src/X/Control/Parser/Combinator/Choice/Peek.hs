module X.Control.Parser.Combinator.Choice.Peek where
import X.Control.Parser
import Data.Maybe
import X.Control.Parser.Combinator.Choice.LookaheadN

-- | Peeks the next character (or Nothing if EOF) and chooses a Parser based on it
peek :: (Maybe Char -> Parser a) -> Parser a
peek = lookaheadN 1 . (. listToMaybe)
