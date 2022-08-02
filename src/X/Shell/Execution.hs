{-# LANGUAGE TupleSections #-}
module X.Shell.Execution where
import X.Data.AST.Statement
import X.Data.ParseError
import qualified X.Data.Context as Ctx
import X.Data.AST.Assignment
import X.Control.Terminal
import X.Shell.Formatting
import Control.Applicative
import X.Control.Parser
import X.Control.Parser.AST.Statement
import X.Control.Parser.Combinator.WithTrailingWhitespace
import X.Control.Parser.Combinator.Complete
import X.Utils.LeftToRight
import X.Data.Value.Evaluate

-- | Parses input, returning Either an error or the result of said input
parseStatement :: String -> Either ParseError Statement
parseStatement input =
    parse (statement @> withTrailingWhitespace @> complete) input |@>| snd

-- | Evaluates a parsed Statement, returning either an error message or the new state and string output
evaluateStatement :: Ctx.Context -> Statement -> (Ctx.Context, String)
evaluateStatement ctx (StmtValue val) =
    evaluateValue val ctx
        @> show
        @> (ctx,)
evaluateStatement state (StmtAssignment (Assignment a val)) =
    let toPrintedStatement x = a <> " <- " <> show x
        newState x = Ctx.put a x state
     in evaluateValue val state
            @> liftA2 (,) newState toPrintedStatement

-- | Executes a parsed Statement, returning the new state and what should be printed to the screen
executeStatement :: Ctx.Context -> Statement -> (Ctx.Context, [PrintCmd])
executeStatement state stmt = evaluateStatement state stmt |@>| makeValueCmds

-- | Given the current state, terminal width, and input string, returns the new state and what should be printed to the screen
execute :: Ctx.Context -> Int -> String -> (Ctx.Context, [PrintCmd])
execute state width line =
    case parseStatement line of
        Left parseError -> (state, makeParseErrorCmds width line parseError)
        Right stmt -> executeStatement state stmt
