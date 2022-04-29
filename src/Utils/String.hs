module Utils.String where

trimLine :: Int -> String -> String
trimLine maxWidth line =
    if length line > maxWidth
        then take (maxWidth - 3) line <> "..."
        else line
