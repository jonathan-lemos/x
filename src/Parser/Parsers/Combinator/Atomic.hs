module Parser.Parsers.Combinator.Atomic where
import Parser.Parsers.Combinator.MapResult
import Parser.Parser
import Data.Bifunctor
import Parser.Error

{- | If the parser fails, sets the input to the original input. In other words, it either parses an entire value or nothing

## Examples

>>> import Parser.Parsers.Text.CharEq

>>> parse (atomic (charEq 'a')) "abc"
Right ("bc",'a')

>>> parse (atomic (charEq 'a')) "bc"
Left (ParseError {reason = "Expected 'a'", currentInput = "bc"})

>>> parse (atomic (charEq 'a')) ""
Left (ParseError {reason = "Expected 'a'", currentInput = ""})

-}
atomic :: Parser a -> Parser a
atomic = mapResultWithInput $ first . setCurrentInput
