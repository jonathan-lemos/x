{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}


module X.Shell.Main where

import Control.Monad.Free
import X.Control.Terminal
import qualified X.Data.Context as Ctx
import X.Shell.Execution
import X.Utils.LeftToRight

-- | Reads a line from the terminal.
readCommand :: Terminal (Maybe String)
readCommand = do
    printCmd $ text "x> "
    inputLine

-- | Using the given state, executes the result of a read operation with the given terminal dimensions.
executeCommand :: Ctx.Context -> Maybe String -> Maybe TerminalDimensions -> Terminal Ctx.Context
executeCommand state (Just cmd) dims = do
    let w = maybe 1000 width dims

    let (newState, ioCmds) = execute state w cmd
    ioCmds |@>| printCmd @> sequence_

    return newState
executeCommand _state Nothing _dims = Free Exit

