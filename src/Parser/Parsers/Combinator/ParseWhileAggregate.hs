module Parser.Parsers.Combinator.ParseWhileAggregate where

import Parser.Parser

{- | Parses while the predicate is `True` for *all of the parsed results*, stopping at the first `False`, returning the list of all the `True` results.

 ## Examples

 $setup
 >>> import Data.Char
 >>> import Parser.Parsers.Text.Char

 >>> parse (parseWhileAggregate (all isDigit) char) "123abc"
 Right ("abc","123")

 >>> parse (parseWhileAggregate (all isDigit) char) "abc"
 Right ("abc","")

 >>> parse (parseWhileAggregate (all isDigit) char) ""
 Right ("","")
-}
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

