module Utils.String where

pluralize :: [String] -> String
pluralize [] = ""
pluralize [x] = x
pluralize [x, y] = x <> " or " <> y
pluralize (x:xs) = x <> ", " <> pluralize xs

trimLine :: Int -> String -> String
trimLine maxWidth line =
    if length line > maxWidth
        then take (maxWidth - 3) line <> "..."
        else line
