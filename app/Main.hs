module Main where

import Lib
import GHC.IO.Handle
import GHC.IO.Handle.FD

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    mainLoop
