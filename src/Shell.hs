module Shell where
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Types.Expression
import Parser.Parsers.Text.Whitespace (whitespace)

execute :: String -> Either String String
execute statement =
    let parser = do
            pt <- arithmeticExpression
            whitespace
            return pt

        parseTree = parse parser statement

    in case parseTree of
        Just ("", value) -> Right . show . evaluate $ value
        _ -> Left "Syntax Error"

prompt :: IO ()
prompt = putStr "x> "
