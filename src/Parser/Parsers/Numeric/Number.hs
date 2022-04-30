module Parser.Parsers.Numeric.Number where

import Control.Applicative
import Parser.Parsers.Numeric.Digit
import Parser.Parser
import Parser.Parsers.Combinator.Conditional
import Data.Char
import Parser.Parsers.Text.Char
import Parser.Parsers.Text.CharEq

{- | Parses an integer. This does not include the sign.

## Examples

>>> parse integer "123abc"
Right ("abc",123)

>>> parse integer "-4"
Left (ParseError {reason = "Expected digit", currentInput = "-4"})

>>> parse integer ""
Left (ParseError {reason = "Expected digit", currentInput = ""})

-}
integer :: Parser Integer
integer = foldl (\a c -> a * 10 + toInteger c) 0 <$> (some digit <|> fail "Expected digit")
