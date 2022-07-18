module Lib where

import X.Shell.Main
import X.Shell.Repl
import X.Control.Terminal
import qualified X.Data.Context as Context

mainLoop :: Terminal ()
mainLoop = repl Context.new readCommand executeCommand
