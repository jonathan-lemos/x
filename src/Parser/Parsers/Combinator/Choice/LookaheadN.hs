module Parser.Parsers.Combinator.Choice.LookaheadN where

import Parser.Parsers.Combinator.Choice.Lookahead
import Parser.Parser

{- | Looks at the remaining input (up to N characters) and chooses a Parser based on it

 ## Examples

 >>> :{
 >>> f "abc" = pure 1
 >>> f "ab"  = pure 2
 >>> f "def" = pure 3
 >>> f _     = pure 4
 >>> :}

 >>> parse (lookaheadN 3 f) "abcdef"
 Right ("abcdef",1)

 >>> parse (lookaheadN 3 f) "ab"
 Right ("ab",2)

 >>> parse (lookaheadN 3 f) "defabc"
 Right ("defabc",3)

 >>> parse (lookaheadN 3 f) ""
 Right ("",4)
-}
lookaheadN :: Int -> (String -> Parser a) -> Parser a
lookaheadN n f = lookahead (f . take n)


