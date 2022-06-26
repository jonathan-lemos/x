module X.Control.Parser.Numeric.CReal where

import Data.Number.CReal
import Control.Applicative
import X.Control.Parser
import X.Control.Parser.Text.CharEq
import X.Control.Parser.Text.CharAny
import X.Control.Parser.Text.Char
import Data.Char
import X.Control.Parser.Combinator.Branch.Conditional

-- | Parses a real number
creal :: Parser CReal
creal = do
    let asList = ((:[]) <$>)

    let digitSeq = some (conditional isDigit char) <|> fail "Expected a sequence of digits"
    let sign = charAny "+-"

    let decPart = asList (charEq '.') <> digitSeq
    let expPart = asList (charAny "eE") <> (asList sign <|> mempty) <> digitSeq

    s <- asList sign <|> mempty
    d <- digitSeq
    dec <- decPart <|> mempty
    e <- expPart <|> mempty

    return $ read (concat [s, d, dec, e])
