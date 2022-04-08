module Parser.Parsers.Literal where

import Parser.Parser
import Parser.Parsers.Char
import Parser.Parsers.Many
import Parser.Parsers.Conditional
import Utils

literal :: String -> Parser String 
literal = mconcat . fmap (\c -> (:[]) <$> conditional char (== c))
