module Lib where

import X.Shell.Main
import X.Data.State.XState
import X.Shell.Repl
import X.Control.Terminal

mainLoop :: Terminal ()
mainLoop = repl newState readCommand executeCommand
