module Parser.Parsers.Combinator.Branch.Check where

import Parser.Error
import Parser.Parser
import Parser.Parsers.Combinator.MapResult

{- | If the given parser succeeds, but the `predicate` returns `False`, fails with the message returned by `valueToMsg`, otherwise passes `p` through.

 ## Examples

 >>> import Data.Char
 >>> import Parser.Parsers.Text.Char

 >>> parse (check isDigit (\c -> show c <> " is not digit") char) "3 abc"
 Right (" abc",'3')

 >>> parse (check isDigit (\c -> show c <> " is not digit") char) "a abc"
 Left (ParseError {reason = "'a' is not digit", currentInput = " abc"})

 >>> parse (check isDigit (\c -> show c <> " is not digit") char) ""
 Left (ParseError {reason = "Expected any character", currentInput = ""})
-}
check :: (a -> Bool) -> (a -> String) -> Parser a -> Parser a
check predicate valueToMsg = mapResultWithInput $ \_input e ->
    e >>= \(newInput, value) ->
        if predicate value
            then Right (newInput, value)
            else Left $ ParseError (valueToMsg value) newInput
