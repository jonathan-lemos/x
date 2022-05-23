module Lib where
import Shell.Shell
import System.Console.Terminal.Size
import Data.Maybe
import IO.IOCmd
import Types.State.XState

repl :: IO ()
repl =
    let loop state = do
            state' <- rep state
            loop state'
    in loop newState
