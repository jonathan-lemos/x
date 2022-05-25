module Lib where
import Shell.Shell
import State.XState

repl :: IO ()
repl =
    let loop state = do
            state' <- rep state
            loop state'
    in loop newState
