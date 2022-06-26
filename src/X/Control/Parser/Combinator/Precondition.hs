{-# LANGUAGE TupleSections #-}
module X.Control.Parser.Combinator.Precondition where
import X.Control.Parser
import Data.Bifunctor

-- | If the first Parser fails, returns Nothing. Otherwise returns the result of both parsers, or the second parser's failure.
precondition :: Parser a -> Parser b -> Parser (Maybe (a, b))
precondition pa pb = Parser $ \s ->
    case parse pa s of
        Left _ -> Right (s, Nothing)
        Right (sa, v) -> second (Just . (v,)) <$> parse pb sa
