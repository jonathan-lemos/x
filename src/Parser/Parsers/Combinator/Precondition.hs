{-# LANGUAGE TupleSections #-}
module Parser.Parsers.Combinator.Precondition where
import Parser.Parser
import Data.Bifunctor

{- | If the first Parser fails, returns Nothing. Otherwise returns the result of both parsers, or the second parser's failure.

 ## Examples

 >>> import Parser.Parsers.Text.CharEq

 >>> parse (precondition (charEq 'a') (charEq 'b')) "abcd"
 Right ("cd",Just ('a','b'))

 >>> parse (precondition (charEq 'a') (charEq 'b')) "bcd"
 Right ("bcd",Nothing)

 >>> parse (precondition (charEq 'a') (charEq 'b')) "acd"
 Left (ParseError {reason = "Expected 'b'", currentInput = "cd"})
-}
precondition :: Parser a -> Parser b -> Parser (Maybe (a, b))
precondition pa pb = Parser $ \s ->
    case parse pa s of
        Left _ -> Right (s, Nothing)
        Right (sa, v) -> second (Just . (v,)) <$> parse pb sa
