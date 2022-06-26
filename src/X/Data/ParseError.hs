module X.Data.ParseError where

-- | Gives the reason and location of a failed parse
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
