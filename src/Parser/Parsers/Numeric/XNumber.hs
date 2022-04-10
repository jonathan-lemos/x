module Parser.Parsers.Numeric.XNumber where

import Types.XNumber
import Parser.Parser
import Parser.Parsers.Numeric.Number
import Control.Applicative

xnumber :: Parser XNumber
xnumber = XReal <$> double <|> XInteger <$> integer
