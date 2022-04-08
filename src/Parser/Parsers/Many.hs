module Parser.Parsers.Many where

import Parser.Parser
import Utils (mapSnd)

many :: Parser t -> Parser [t]
many (Parser f) = Parser $ \s ->
    case f s of
        Just (s, v) -> mapSnd (v:) <$> parse (many (Parser f)) s
        Nothing -> Just (s, [])
