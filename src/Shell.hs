module Shell where

import IO.IOCmd
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parsers.Text.Whitespace (whitespace)
import System.Console.ANSI
import Types.Expression
import Utils.String

makeErrorFooter :: Int -> String -> [IOCmd]
makeErrorFooter maxWidth content =
    let contentLines = take 5 $ trimLine maxWidth <$> lines content
        errorLine = head contentLines
        followingLines = tail contentLines
     in [ line errorLine
        , coloredLine [SetColor Foreground Vivid Red] "^"
        ]
            <> fmap (coloredLine [SetColor Foreground Dull White]) followingLines

execute :: String -> Either ParseError String
execute statement =
    let parser = do
            pt <- arithmeticExpression
            whitespace
            return pt

        parseTree = parse parser statement
     in case parseTree of
            Right ("", value) -> Right $ show value
            Right (remaining, _) -> Left $ ParseError "Unexpected end of parse" remaining
            Left pe -> Left pe

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

prompt :: IO ()
prompt = putStr "x> "
