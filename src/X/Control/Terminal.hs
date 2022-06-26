module X.Control.Terminal where

import Control.Monad.Free
import System.Console.ANSI
import qualified System.Console.Terminal.Size as ST
import System.Exit

data TerminalDimensions = TerminalDimensions
    { height :: Int
    , width :: Int
    }

data TerminalF a
    = InputLine (Maybe String -> a)
    | Dimensions (Maybe TerminalDimensions -> a)
    | Print PrintCmd a
    | Exit

instance Functor TerminalF where
    fmap f (InputLine s) = InputLine $ f . s
    fmap f (Dimensions d) = Dimensions $ f . d
    fmap f (Print pc a) = Print pc $ f a
    fmap _ Exit = Exit

type Terminal = Free TerminalF

inputLine :: Terminal (Maybe String)
inputLine = liftF $ InputLine id

dimensions :: Terminal (Maybe TerminalDimensions)
dimensions = liftF $ Dimensions id

printCmd :: PrintCmd -> Terminal ()
printCmd cmd = liftF $ Print cmd ()

exit :: Terminal a
exit = liftF Exit

run :: Terminal a -> IO a
run (Pure a) = return a
run (Free (InputLine fn)) = getLine >>= (run . fn) . Just
run (Free (Dimensions fn)) =
    let mapWindow w = TerminalDimensions{height = ST.height w, width = ST.width w}
        getDims = (fmap . fmap) mapWindow ST.size
     in getDims >>= run . fn
run (Free (Print p a)) =
    let printCmd (PrintCmd sgr txt) = do
            setSGR sgr
            putStr txt
            setSGR [Reset]
     in printCmd p >> run a
run (Free Exit) = exitSuccess

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
