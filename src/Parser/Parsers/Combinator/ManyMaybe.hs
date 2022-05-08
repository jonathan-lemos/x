module Parser.Parsers.Combinator.ManyMaybe where

import Parser.Parser

{- | Runs the underlying parser until it returns Nothing, collecting the Just values. If the underlying parser fails, manyMaybe passes the error through.
-}
manyMaybe :: Parser (Maybe a) -> Parser [a]
manyMaybe p = Parser $ \state input ->
    let go state input buf =
            case parse p state input of
                Left e -> Left e
                Right (newInput, Just v) -> go state newInput (buf ++ [v])
                Right (newInput, Nothing) -> Right (newInput, buf)
     in go state input []
