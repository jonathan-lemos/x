module Shell where

import IO.IOCmd
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parsers.Text.Whitespace
import System.Console.ANSI
import Types.Expression
import Utils.String

-- | Commands that print the erroring input and its location given the `terminal width` and `remaining input`
makeErrorFooter :: Int -> String -> [IOCmd]
makeErrorFooter maxWidth content =
    let contentLines = take 5 $ trimLine maxWidth <$> lines content
        errorLine = head contentLines
        followingLines = tail contentLines
     in [ line errorLine
        , coloredLine [SetColor Foreground Vivid Red] "^"
        ]
            <> fmap (coloredLine [SetColor Foreground Dull White]) followingLines

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
printResult :: Int -> Either ParseError String -> [IOCmd]
printResult _width (Right value) =
    [coloredLine [SetColor Foreground Vivid Blue] value]
printResult width (Left (ParseError reason currentInput)) =
    [ coloredText [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] "error: "
    , coloredText [SetColor Foreground Vivid Red] reason
    , newline
    , newline
    ]
        <> makeErrorFooter width currentInput

-- | Given the terminal width and a line, produces IO Commands to output
execute :: Int -> String -> [IOCmd]
execute width line = printResult width $ calculate line

-- | Prints the shell prompt
prompt :: IO ()
prompt = putStr "x> "
