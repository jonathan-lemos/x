module Parser.Parsers.Combinator.Peek where
import Parser.Parser
import Parser.Parsers.Combinator.Lookahead
import Data.Maybe

peek :: (Maybe Char -> Parser a) -> Parser a
peek = lookahead 1 . (. listToMaybe)
