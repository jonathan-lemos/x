module Parser.Error where

data ParseError = ParseError {
    reason :: String,
    currentInput :: String
} deriving (Show, Eq)

setReason :: String -> ParseError -> ParseError
setReason s p = p { reason = s }

