module Lib where
import Shell
import System.Console.Terminal.Size
import Data.Maybe
import IO.IOCmd (printCmd)

repl :: IO ()
repl = do
    prompt
    line <- getLine
    termWidth <- maybe (1024 :: Int) width <$> size

    let ioCmds = execute termWidth line
    sequence_ $ printCmd <$> ioCmds

    repl
