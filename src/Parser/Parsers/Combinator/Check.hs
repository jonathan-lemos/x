module Parser.Parsers.Combinator.Check where

import Control.Monad
import Data.Bifunctor
import Parser.Error
import Parser.Parser



{- | If the given parser succeeds, but the `predicate` returns `False`, fails with the message returned by `valueToMsg`, otherwise passes `p` through.

 ## Examples

 $setup
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
check predicate valueToMsg p =
    Parser $
        parse p >=> \(input, value) ->
            if predicate value
                then Right (input, value)
                else Left $ ParseError (valueToMsg value) input
