module IO.IOCmd where

import System.Console.ANSI
import System.Console.Terminal.Size

data IOCmd = IOCmd [SGR] String
    deriving (Eq, Show)

coloredText :: [SGR] -> String -> IOCmd
coloredText = IOCmd

text :: String -> IOCmd
text = IOCmd []

coloredLine :: [SGR] -> String -> IOCmd
coloredLine sgr text = coloredText sgr (text <> "\n")

line :: String -> IOCmd
line = coloredLine []

newline :: IOCmd
newline = line ""

printCmd :: IOCmd -> IO ()
printCmd (IOCmd sgr text) = do
    setSGR sgr
    putStr text
    setSGR [Reset]
