module Parser.Parsers.Combinator.ParseWhile where
import Parser.Parser
import Utils

parseWhile :: Parser t -> Parser [t]
parseWhile p = Parser $ \s ->
    case parse p s of 
        Just (r, v) -> mapSnd (v:) <$> parse (parseWhile p) r
        Nothing -> Just (s, [])
