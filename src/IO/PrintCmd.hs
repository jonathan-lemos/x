module IO.PrintCmd where

import System.Console.ANSI

{- | Represents a print to be done.
 Feed these to `printCmd` to actually execute them.
-}
data PrintCmd = PrintCmd [SGR] String
    deriving (Eq)

instance Show PrintCmd where
    show (PrintCmd _ s) = s

-- | Makes a command that prints text with the given ANSI metadata. No newline is included.
coloredText :: [SGR] -> String -> PrintCmd
coloredText = PrintCmd

-- | Makes a command that prints plain text. No newline is included.
text :: String -> PrintCmd
text = PrintCmd []

-- | Makes a command that prints text and a newline with the given ANSI metadata.
coloredLine :: [SGR] -> String -> PrintCmd
coloredLine sgr txt = coloredText sgr (txt <> "\n")

-- | Makes a command that prints text and a newline.
line :: String -> PrintCmd
line = coloredLine []

-- | A command that prints a newline
newline :: PrintCmd
newline = line ""

-- | Prints a command
printCmd :: PrintCmd -> IO ()
printCmd (PrintCmd sgr txt) = do
    setSGR sgr
    putStr txt
    setSGR [Reset]
