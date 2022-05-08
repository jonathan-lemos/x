{-# LANGUAGE TupleSections #-}

module Shell.Shell where

import Control.Applicative
import Data.Bifunctor
import IO.IOCmd
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parsers.AST.Statement
import Parser.Parsers.Text.Whitespace
import Shell.Formatting
import State.State
import System.Console.ANSI
import System.Console.Terminal.Size
import Types.AST.Assignment
import Types.AST.Statement
import Types.Evaluatable.Evaluatable
import Utils.String

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
                then Left $ ParseError "Unexpected end of parse" r
                else Right v

-- | Executes a parsed Statement, returning either an error message or the new state and string output
executeStatement :: XState -> Statement -> Either String (XState, String)
executeStatement state (StmtAssignment (Assignment a expr)) =
    liftA2 (,) (putVar state a) (((a <> " = ") <>) . show) <$> evaluate expr state
executeStatement state (StmtExpr expr) =
    liftA2 (,) (Right state) (show <$> evaluate expr state)

-- | Given the terminal width and a line, produces the new state (or the same state on error), and IOCmds
execute :: XState -> Int -> String -> (XState, [IOCmd])
execute state width line =
    either
        (state,)
        id
        ( first (makeParseErrorCmds width line) (parseCommand line)
            >>= bimap makeErrorMessageCmds (second makeValueCmds) . executeStatement state
        )

-- | Prints the shell prompt
prompt :: IO ()
prompt = putStr "x> "

rep :: XState -> IO XState
rep state = do
    prompt
    line <- getLine
    termWidth <- maybe (1024 :: Int) width <$> size

    let (newState, ioCmds) = execute state termWidth line
    sequence_ $ printCmd <$> ioCmds

    return newState
