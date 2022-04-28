module Utils.String where

quote :: String -> String
quote s = "\"" <> s <> "\""

quoteShow :: (Show a) => a -> String
quoteShow = quote . show

trimLine :: Int -> String -> String
trimLine maxWidth line =
    if length line > maxWidth
        then take (maxWidth - 3) line <> "..."
        else line
