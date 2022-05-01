module IO.IOCmd where

import System.Console.ANSI
import System.Console.Terminal.Size

{- | Represents a print to be done.
 Feed these to `printCmd` to actually execute them.
-}
data IOCmd = IOCmd [SGR] String
    deriving (Eq)

instance Show IOCmd where
    show (IOCmd _ s) = s

-- | Makes a command that prints text with the given ANSI metadata. No newline is included.
coloredText :: [SGR] -> String -> IOCmd
coloredText = IOCmd

-- | Makes a command that prints plain text. No newline is included.
text :: String -> IOCmd
text = IOCmd []

-- | Makes a command that prints text and a newline with the given ANSI metadata.
coloredLine :: [SGR] -> String -> IOCmd
coloredLine sgr text = coloredText sgr (text <> "\n")

-- | Makes a command that prints text and a newline.
line :: String -> IOCmd
line = coloredLine []

-- | A command that prints a newline
newline :: IOCmd
newline = line ""

-- | Prints a command
printCmd :: IOCmd -> IO ()
printCmd (IOCmd sgr text) = do
    setSGR sgr
    putStr text
    setSGR [Reset]
