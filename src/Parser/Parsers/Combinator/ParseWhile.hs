module Parser.Parsers.Combinator.ParseWhile where
import Parser.Parser
import Data.Bifunctor

parseWhile :: Parser t -> Parser [t]
parseWhile p = Parser $ \s ->
    case parse p s of 
        Just (r, v) -> second (v:) <$> parse (parseWhile p) r
        Nothing -> Just (s, [])
