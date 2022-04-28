module Parser.Parsers.Combinator.ParseWhileAggregate where

import Parser.Parser

-- | Parses while the predicate is `True` for *all of the parsed results*, stopping at the first `False`, returning the list of all the `True` results.
parseWhileAggregate :: ([t] -> Bool) -> Parser t -> Parser [t]
parseWhileAggregate _f _p =
  let pwa p f buf s = case parse p s of
        Right (r, v) ->
          let newBuf = buf ++ [v] in
            if f newBuf
              then pwa p f newBuf r
              else Right (s, buf)
        Left _ -> Right (s, buf)
   in Parser $ pwa _p _f []

