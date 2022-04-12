module Parser.Parsers.Text.Literal where

import Parser.Parser
import Parser.Parsers.Text.Char
import Parser.Parsers.Combinator.Conditional
import Utils

literal :: String -> Parser String 
literal = mconcat . fmap (\c -> (:[]) <$> conditional char (== c))
