module Parser.Parsers.Combinator.Choice.Peek where
import Parser.Parser
import Data.Maybe
import Parser.Parsers.Combinator.Choice.LookaheadN

{- | Peeks the next character (or Nothing if EOF) and chooses a Parser based on it

 ## Examples

 >>> :{
 >>> f (Just 'a') = pure 1
 >>> f (Just 'b') = pure 2
 >>> f Nothing    = pure 3
 >>> f x          = pure 4
 >>> :}

 >>> parse (peek f) "abc"
 Right ("abc",1)

 >>> parse (peek f) "bc"
 Right ("bc",2)

 >>> parse (peek f) ""
 Right ("",3)

 >>> parse (peek f) "qqq"
 Right ("qqq",4)
-}
peek :: (Maybe Char -> Parser a) -> Parser a
peek = lookaheadN 1 . (. listToMaybe)
