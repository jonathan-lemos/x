module Shell.Shell where

import IO.IOCmd
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parsers.Text.Whitespace
import System.Console.ANSI
import Types.Expression
import Utils.String
import Control.Applicative
import Shell.Formatting


-- | Calculates the result of input, returning Either an error or the result of said input
calculate :: String -> Either ParseError String
calculate statement =
    let parser = do
            pt <- arithmeticExpression
            whitespace
            return pt

        parseTree = parse parser statement
     in case parseTree of
            Right ("", value) -> Right $ show (evaluate value)
            Right (remaining, _) -> Left $ ParseError "Unexpected end of parse" remaining
            Left pe -> Left pe

-- | Given the `terminal width` result of `execute`, makes commands for printing it
printResult :: Int -> String -> Either ParseError String -> [IOCmd]
printResult _width _original (Right value) =
    makeValueCmds value
printResult width original (Left (ParseError reason currentInput)) =
    makeErrorMessageCmds reason <> [newline] <> makeErrorLocationCmds width original currentInput

-- | Given the terminal width and a line, produces IOCmds
execute :: Int -> String -> [IOCmd]
execute width line = printResult width line $ calculate line

-- | Prints the shell prompt
prompt :: IO ()
prompt = putStr "x> "
