module Parser.Parsers.Conditional where

import Control.Monad
import Parser.Parser

conditional :: Parser t -> (t -> Bool) -> Parser t
conditional (Parser p) f = Parser $ transform <=< p where
    transform (input, value) =
        if f value then Just (input, value) else Nothing
