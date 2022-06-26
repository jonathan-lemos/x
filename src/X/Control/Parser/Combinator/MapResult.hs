module X.Control.Parser.Combinator.MapResult where
import X.Data.ParseError
import X.Control.Parser

-- | Maps the result of a parser, passing the input as well as the parser's result to the mapper.
mapResultWithInput :: (String -> Either ParseError (String, a) -> Either ParseError (String, b)) -> Parser a -> Parser b
mapResultWithInput f p = Parser $ \input -> f input $ parse p input

-- | Maps the result of a parser
mapResult :: (Either ParseError (String, a) -> Either ParseError (String, a)) -> Parser a -> Parser a
mapResult = mapResultWithInput . const
