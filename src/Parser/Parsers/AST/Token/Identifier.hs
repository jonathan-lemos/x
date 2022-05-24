module Parser.Parsers.AST.Token.Identifier where

import Control.Applicative
import Data.Char
import Parser.Parser
import Parser.Parsers.Combinator.Check
import Parser.Parsers.Text.Char
import Parser.Parsers.Combinator.Atomic
import Parser.Parsers.Combinator.Conditional

identifier :: Parser String
identifier =
    some (conditional isAlpha char) <|> fail "Expected an A-z character"
