module X.Control.Parser.Numeric.Digit where

import X.Control.Parser
import Data.Char
import X.Control.Parser.Combinator.Branch.Conditional
import X.Control.Parser.Text.Char
import Control.Applicative

-- | Parses a 0-9 digit
digit :: Parser Int
digit = read . (:[]) <$> conditional isDigit char <|> fail "Expected digit"
