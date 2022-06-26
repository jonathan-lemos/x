module X.Shell.Repl where

import X.Control.Terminal

-- Reads, evaluates, and prints given a state, command, and function taking (state, command, dimensions) and returning a new state.
rep :: s -> Terminal c -> (s -> c -> Maybe TerminalDimensions -> Terminal s) -> Terminal s
rep state readCommand executeCommand = do
    cmd <- readCommand
    ts <- dimensions
    executeCommand state cmd ts

-- `rep`'s in a loop, terminating only with an `Exit` command from the Terminal.
repl :: s -> Terminal c -> (s -> c -> Maybe TerminalDimensions -> Terminal s) -> Terminal ()
repl state readCommand executeCommand = do
    newState <- rep state readCommand executeCommand
    repl newState readCommand executeCommand
