module Parser.Parsers.Combinator.ParseWhileAggregate where

import Parser.Parser

{- | Parses while the predicate is `True` for *all of the parsed results*, stopping at the first `False`, returning the list of all the `True` results.

 ## Examples

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
parseWhileAggregate f p =
  let pwa buf state input = case parse p state input of
        Right (newInput, value) ->
          let newBuf = buf ++ [value] in
            if f newBuf
              then pwa newBuf state newInput
              else Right (input, buf)
        Left _ -> Right (input, buf)
   in Parser $ pwa []

