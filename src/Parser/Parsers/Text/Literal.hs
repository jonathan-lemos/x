module Parser.Parsers.Text.Literal where

import Parser.Parser
import Parser.Parsers.Text.Char
import Parser.Parsers.Combinator.Conditional
import Utils
import Parser.Parsers.Text.CharEq

literal :: String -> Parser String
literal = mconcat . fmap (fmap (: []) . charEq)
