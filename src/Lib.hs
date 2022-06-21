module Lib where
import Shell.Shell
import State.XState
import Shell.Repl.Repl
import IO.Terminal

mainLoop :: Terminal ()
mainLoop = repl newState readCommand executeCommand
