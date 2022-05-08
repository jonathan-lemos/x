module Parser.Error where

data ParseError = ParseError
    { reason :: String
    , currentInput :: String
    }
    deriving (Show, Eq)

-- | Sets the `currentInput` field of a `ParseError`
setCurrentInput :: String -> ParseError -> ParseError
setCurrentInput s p = p{currentInput = s}

-- | Sets the `reason` field of a `ParseError`
setReason :: String -> ParseError -> ParseError
setReason s p = p{reason = s}
