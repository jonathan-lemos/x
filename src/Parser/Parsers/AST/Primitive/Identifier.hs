module Parser.Parsers.AST.Primitive.Identifier where

import Control.Applicative
import Data.Char
import Parser.Parser
import Parser.Parsers.Combinator.Check
import Parser.Parsers.Text.Char

identifier :: Parser String
identifier =
    some
        ( check
            isAlpha
            (\c -> "Expected an identifier, which is a sequence of lowercase characters (" <> show c <> " is not)")
            char
        )