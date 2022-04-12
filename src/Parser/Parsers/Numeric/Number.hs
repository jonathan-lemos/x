module Parser.Parsers.Numeric.Number where

import Control.Applicative
import Parser.Parsers.Numeric.Digit
import Parser.Parser
import Parser.Parsers.Combinator.Conditional
import Data.Char
import Parser.Parsers.Text.Char

integer :: Parser Integer
integer = foldl (\a c -> a * 10 + toInteger c) 0 <$> some digit

double :: Parser Double
double = do
    let digitString = some (conditional char isDigit)

    whole <- digitString <|> return "0"
    point <- conditional char (== '.')
    decimal <- digitString

    return $ read $ concat [whole, [point], decimal]
