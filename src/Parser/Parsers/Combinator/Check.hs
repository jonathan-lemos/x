module Parser.Parsers.Combinator.Check where

import Parser.Parser
import Parser.Error
import Data.Bifunctor
import Control.Monad

-- | If the given parser succeeds, but the `predicate` returns `False`, fails with the message returned by `valueToMsg`, otherwise passes `p` through.
check :: (a -> Bool) -> (a -> String) -> Parser a -> Parser a
check predicate valueToMsg p =
    Parser $ parse p >=> \(input, value) ->
        if predicate value
            then Right (input, value)
            else Left $ ParseError (valueToMsg value) input

