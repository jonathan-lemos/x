module X.Control.Parser.Combinator.Branch.Check where

import X.Data.ParseError
import X.Control.Parser
import X.Control.Parser.Combinator.MapResult

-- | If the given parser succeeds, but the `predicate` returns `False`, fails with the message returned by `valueToMsg`, otherwise passes `p`'s result through.
check :: (a -> Bool) -> (a -> String) -> Parser a -> Parser a
check predicate valueToMsg = mapResultWithInput $ \_input e ->
    e >>= \(newInput, value) ->
        if predicate value
            then Right (newInput, value)
            else Left $ ParseError (valueToMsg value) newInput
