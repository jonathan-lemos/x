module Parser.Parsers.Combinator.Conditional where

import Control.Monad
import Parser.Parser

conditional :: Parser t -> (t -> Bool) -> Parser t
conditional p f =
    p
        { parse =
            parse p >=> \(input, value) ->
                if f value
                    then Just (input, value)
                    else Nothing
        }
