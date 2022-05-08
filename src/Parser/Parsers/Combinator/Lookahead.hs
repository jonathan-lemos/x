module Parser.Parsers.Combinator.Lookahead where

import Control.Monad
import Parser.Parser

{- | Looks at the remaining input and chooses a Parser based on it

 ## Examples

 >>> import Data.List
 >>> :{
 >>> f s
 >>>  | "abc" `isPrefixOf` s = pure 1
 >>>  | "ab"  `isPrefixOf` s = pure 2
 >>>  | "def" `isPrefixOf` s = pure 3
 >>>  | otherwise            = pure 4
 >>> :}

 >>> parse (lookahead f) "abcdef"
 Right ("abcdef",1)

 >>> parse (lookahead f) "abd"
 Right ("abd",2)

 >>> parse (lookahead f) "defabc"
 Right ("defabc",3)

 >>> parse (lookahead f) ""
 Right ("",4)
-}
lookahead :: (String -> Parser a) -> Parser a
lookahead f = join . Parser $ \s -> Right (s, f s)
