module Parser.Parsers.Combinator.ParseWhile where
import Parser.Parser
import Utils

parseWhile :: Parser t -> (t -> Bool) -> Parser [t]
parseWhile (Parser p) f = Parser $ \s ->
    case p s of 
        Just (r, v) ->
            if f v then mapSnd (v:) <$> parse (parseWhile (Parser p) f) r else Just (s, [])
        Nothing -> Just (s, [])
