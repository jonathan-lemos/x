module Parser.Parsers.Combinator.MapResult where
import Parser.Error
import Parser.Parser

mapResultWithInput :: (String -> Either ParseError (String, a) -> Either ParseError (String, b)) -> Parser a -> Parser b
mapResultWithInput f p = Parser $ \state input ->
    f input $ parse p state input

mapResult :: (Either ParseError (String, a) -> Either ParseError (String, b)) -> Parser a -> Parser b
mapResult f = mapResultWithInput $ const f
