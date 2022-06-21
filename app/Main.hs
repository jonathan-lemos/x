module Main where

import Lib
import IO.Terminal
import GHC.IO.Handle
import GHC.IO.Handle.FD

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    run $ mainLoop
