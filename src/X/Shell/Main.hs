{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}

module X.Shell.Main where

import Control.Applicative
import Control.Monad.Free
import Data.List
import X.Control.Parser
import X.Control.Parser.AST.Statement
import X.Control.Parser.Combinator.Complete
import X.Control.Parser.Combinator.WithTrailingWhitespace
import X.Control.Terminal
import X.Control.Try
import X.Data.AST.Assignment
import X.Data.AST.Statement
import X.Data.ParseError
import X.Data.State.XState
import X.Evaluation.ToValue
import X.Shell.Formatting
import X.Utils.Function
import X.Utils.Functor

-- | Parses input, returning Either an error or the result of said input
parseCommand :: String -> Either ParseError Statement
parseCommand input =
    parse (statement >$ withTrailingWhitespace >$ complete) input >$> snd

-- | Evaluates a parsed Statement, returning either an error message or the new state and string output
evaluateStatement :: XState -> Statement -> Try (XState, String)
evaluateStatement state (StmtExpr expr) =
    toValue expr state
        >$> show
        >$> (state,)
evaluateStatement state (StmtAssignment (Assignment a expr)) =
    let toPrintedStatement x = a <> " <- " <> show x
        newState x = putVar a x state
     in toValue expr state
            >$> liftA2 (,) newState toPrintedStatement

-- | Executes a parsed Statement, returning the new state and what should be printed to the screen
executeStatement :: XState -> Statement -> (XState, [PrintCmd])
executeStatement state stmt =
    case evaluateStatement state stmt of
        Success (newState, line) -> (newState, makeValueCmds line)
        Failures fs ->
            fs >$> makeErrorMessageCmds
                >$ intercalate [newline]
                >$ (state,)

-- | Given the current state, terminal width, and input string, returns the new state and what should be printed to the screen
execute :: XState -> Int -> String -> (XState, [PrintCmd])
execute state width line =
    case parseCommand line of
        Left parseError -> (state, makeParseErrorCmds width line parseError)
        Right stmt -> executeStatement state stmt

-- | Reads a line from the terminal.
readCommand :: Terminal (Maybe String)
readCommand = do
    printCmd $ text "x> "
    inputLine

-- | Using the given state, executes the result of a read operation with the given dimensions.
executeCommand :: XState -> Maybe String -> Maybe TerminalDimensions -> Terminal XState
executeCommand state (Just cmd) dims = executeJust state cmd dims
executeCommand _state Nothing _dims = Free Exit

-- | Using the given state, executes a line with the given dimensions.
executeJust :: XState -> String -> Maybe TerminalDimensions -> Terminal XState
executeJust state cmd dimensions = do
    let w = maybe 1000 width dimensions

    let (newState, ioCmds) = execute state w cmd
    ioCmds >$> printCmd >$ sequence_

    return newState
