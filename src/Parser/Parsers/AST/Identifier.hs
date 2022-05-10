module Parser.Parsers.AST.Identifier where

import Control.Applicative
import Data.Char
import Parser.Parser
import Parser.Parsers.Combinator.Check
import Parser.Parsers.Text.Char
import Parser.Parsers.Combinator.Atomic

identifier :: Parser String
identifier =
    atomic . some $
        check
            isAlpha
            (\c -> "Expected an identifier, which is a sequence of lowercase characters (" <> show c <> " is not)")
            char
