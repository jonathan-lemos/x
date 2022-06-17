module IO.Terminal where
import System.Console.ANSI
import qualified System.Console.Terminal.Size as ST

data TerminalDimensions = TerminalDimensions {
    height :: Int,
    width :: Int
}

class Monad m => Terminal m where
    inputLine :: m (Maybe String)
    dimensions :: m (Maybe TerminalDimensions)
    printCmd :: PrintCmd -> m ()

instance Terminal IO where
    inputLine = Just <$> getLine
    dimensions = (fmap . fmap) (\w -> TerminalDimensions (ST.height w) (ST.width w)) ST.size
    printCmd (PrintCmd sgr txt) = do
        setSGR sgr
        putStr txt
        setSGR [Reset]

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
