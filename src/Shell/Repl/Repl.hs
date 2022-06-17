module Shell.Repl.Repl where

import IO.Terminal

rep :: Terminal m => s -> m c -> (s -> c -> Maybe TerminalDimensions -> m s) -> m s
rep state readCommand executeCommand = do
    cmd <- readCommand
    ts <- dimensions
    executeCommand state cmd ts

repl :: Terminal m => s -> m c -> (s -> c -> Maybe TerminalDimensions -> m s) -> m ()
repl state readCommand executeCommand = do
    newState <- rep state readCommand executeCommand
    repl newState readCommand executeCommand
