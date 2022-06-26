module X.Control.Parser.Combinator.ManyMaybe where

import X.Control.Parser

-- | Runs the underlying parser until it returns Nothing, collecting the Just values. If the underlying parser fails, manyMaybe passes the error through.
manyMaybe :: Parser (Maybe a) -> Parser [a]
manyMaybe p = Parser $ \s ->
    let go input buf =
            case parse p input of
                Left e -> Left e
                Right (newInput, Just v) -> go newInput (buf ++ [v])
                Right (newInput, Nothing) -> Right (newInput, buf)
     in go s []
