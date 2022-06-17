module Lib where
import Shell.Shell
import State.XState
import Shell.Repl.Repl

mainLoop :: IO ()
mainLoop = repl newState readCommand executeCommand
