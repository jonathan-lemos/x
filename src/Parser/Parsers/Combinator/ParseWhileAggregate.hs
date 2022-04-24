module Parser.Parsers.Combinator.ParseWhileAggregate where

import Parser.Parser

parseWhileAggregate :: Parser t -> ([t] -> Bool) -> Parser [t]
parseWhileAggregate p f =
  let pwa p f buf s = case parse p s of
        Just (r, v) ->
          let newBuf = buf ++ [v] in
            if f newBuf
              then pwa p f newBuf r
              else Just (s, buf)
        Nothing -> Just (s, buf)
   in Parser {
     parse = pwa p f [],
     name = "many " <> name p,
     expected = fmap ("many " <>) (expected p)
   }
