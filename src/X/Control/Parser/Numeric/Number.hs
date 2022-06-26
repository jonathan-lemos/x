module X.Control.Parser.Numeric.Number where

import Control.Applicative
import X.Control.Parser.Numeric.Digit
import X.Control.Parser

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
