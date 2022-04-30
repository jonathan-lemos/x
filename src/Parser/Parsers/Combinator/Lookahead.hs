module Parser.Parsers.Combinator.Lookahead where

import Control.Monad
import Parser.Parser

{- | Looks at the the next n characters, or the entire remaining input (whichever is shorter), and chooses a Parser based on the characters seen

 ## Examples
-}
lookahead :: Int -> (String -> Parser a) -> Parser a
lookahead n f =
    let select = f . take n
     in join . Parser $ Right . ((,) <*> select)
