{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Shell.Shell where

import Control.Applicative
import Data.Bifunctor
import IO.Terminal
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.Statement
import Parser.Parsers.Text.Whitespace
import Shell.Formatting
import State.XState
import Types.AST.Assignment
import Types.AST.Statement
import Evaluation.ToValue

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

readCommand :: Terminal (Maybe String)
readCommand = do
    printCmd $ text "x> "
    inputLine

executeCommand :: XState -> Maybe String -> Maybe TerminalDimensions -> Terminal XState
executeCommand state (Just cmd) dims = executeJust state cmd dims
executeCommand state Nothing dims = executeNothing state dims

executeJust :: XState -> String -> Maybe TerminalDimensions -> Terminal XState
executeJust state cmd dimensions = do
    let w = maybe 1000 width dimensions

    let (newState, ioCmds) = execute state w cmd
    sequence_ $ printCmd <$> ioCmds

    return newState

executeNothing :: XState -> Maybe TerminalDimensions -> Terminal XState
executeNothing state _dimensions = return state
