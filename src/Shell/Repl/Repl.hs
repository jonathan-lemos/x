module Shell.Repl.Repl where

import IO.Terminal

rep :: s -> Terminal c -> (s -> c -> Maybe TerminalDimensions -> Terminal s) -> Terminal s
rep state readCommand executeCommand = do
    cmd <- readCommand
    ts <- dimensions
    executeCommand state cmd ts

repl :: s -> Terminal c -> (s -> c -> Maybe TerminalDimensions -> Terminal s) -> Terminal ()
repl state readCommand executeCommand = do
    newState <- rep state readCommand executeCommand
    repl newState readCommand executeCommand
