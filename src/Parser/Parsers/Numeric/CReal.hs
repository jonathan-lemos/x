module Parser.Parsers.Numeric.CReal where

import Data.Number.CReal
import Control.Applicative
import Parser.Parsers.Numeric.Digit
import Parser.Parser
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Text.CharAny
import Parser.Parsers.Text.Char
import Data.Char
import Parser.Parsers.Combinator.Conditional
import Control.Monad
import Parser.Parsers.Combinator.Plus

{- |
 Parses a real number

 ## Examples

 >>> parse creal "45"
 Right ("",45.0)

 >>> parse creal "-123.456"
 Right ("",-123.456)

 >>> parse creal "2e-2"
 Right ("",0.02)

 >>> parse creal "foo"
 Left (ParseError {reason = "Expected a sequence of digits", currentInput = "foo"})

 >>> parse creal ""
 Left (ParseError {reason = "Expected a sequence of digits", currentInput = ""})
-}
creal :: Parser CReal
creal = do
    let asList = ((:[]) <$>)

    let digitSeq = plus (conditional isDigit char) <|> fail "Expected a sequence of digits"
    let sign = charAny "+-"

    let decPart = asList (charEq '.') <> digitSeq
    let expPart = asList (charAny "eE") <> (asList sign <|> mempty) <> digitSeq

    s <- asList sign <|> mempty
    d <- digitSeq
    dec <- decPart <|> mempty
    e <- expPart <|> mempty

    return $ read (concat [s, d, dec, e])
