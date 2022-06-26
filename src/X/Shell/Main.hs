{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module X.Shell.Main where

import Control.Applicative
import Data.Bifunctor
import X.Control.Terminal
import X.Data.ParseError
import X.Control.Parser
import X.Control.Parser.AST.Statement
import X.Control.Parser.Text.Whitespace
import X.Shell.Formatting
import X.Data.State.XState
import X.Data.AST.Assignment
import X.Data.AST.Statement
import X.Evaluation.ToValue
import Control.Monad.Free

-- | Parses input, returning Either an error or the result of said input
parseCommand :: String -> Either ParseError Statement
parseCommand input =
    let parser = do
            pt <- statement
            whitespace
            return pt

        parseTree = parse parser input
     in parseTree >>= \(r, v) ->
            if null r
                then Right v
                else Left $ ParseError "Unexpected end of parse" r

-- | Executes a parsed Statement, returning either an error message or the new state and string output
executeStatement :: XState -> Statement -> Either String (XState, String)
executeStatement state (StmtAssignment (Assignment a expr)) =
    liftA2 (,) (\x -> putVar a x state) (((a <> " <- ") <>) . show) <$> toValue expr state
executeStatement state (StmtExpr expr) =
    liftA2 (,) (Right state) (show <$> toValue expr state)

-- | Given the terminal width and a line, produces the new state (or the same state on error), and PrintCmds
execute :: XState -> Int -> String -> (XState, [PrintCmd])
execute state width line =
    either
        (state,)
        id
        ( first (makeParseErrorCmds width line) (parseCommand line)
            >>= bimap makeErrorMessageCmds (second makeValueCmds) . executeStatement state
        )

-- | Reads a line from the terminal, if one exists
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
    sequence_ $ printCmd <$> ioCmds

    return newState
