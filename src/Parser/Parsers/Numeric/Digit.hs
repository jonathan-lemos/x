module Parser.Parsers.Numeric.Digit where

import Parser.Parser
import Data.Char
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.Char
import Control.Applicative

{- | Parses a 0-9 digit

## Examples

>>> parse digit "123abc"
Right ("23abc",1)

>>> parse digit "foo"
Left (ParseError {reason = "Expected digit", currentInput = "foo"})

>>> parse digit ""
Left (ParseError {reason = "Expected digit", currentInput = ""})

-}
digit :: Parser Int
digit = read . (:[]) <$> conditional isDigit char <|> fail "Expected digit"
