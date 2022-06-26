module X.Control.Parser.AST.Token.Identifier where

import Control.Applicative
import Data.Char
import X.Control.Parser
import X.Control.Parser.Text.Char
import X.Control.Parser.Combinator.Branch.Conditional

-- | Matches an identifier, which uniquely identifies
identifier :: Parser String
identifier =
    some (conditional isAlpha char) <|> fail "Expected an A-z character"
