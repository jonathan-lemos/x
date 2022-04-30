module Parser.Error where

data ParseError = ParseError
    { reason :: String
    , currentInput :: String
    }
    deriving (Show, Eq)

-- | Sets the `reason` field of a `ParseError`
setReason :: String -> ParseError -> ParseError
setReason s p = p{reason = s}
