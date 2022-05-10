module Parser.Parsers.Combinator.MapResult where
import Parser.Error
import Parser.Parser

mapResultWithInput :: (String -> Either ParseError (String, a) -> Either ParseError (String, a)) -> Parser a -> Parser a
mapResultWithInput f p = Parser $ \input -> f input $ parse p input

mapResult :: (Either ParseError (String, a) -> Either ParseError (String, a)) -> Parser a -> Parser a
mapResult = mapResultWithInput . const
