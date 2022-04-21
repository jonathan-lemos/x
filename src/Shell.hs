module Shell where
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import Types.Expression

execute :: String -> Either String String
execute statement =
    let parseTree = parse arithmeticExpression statement
    in case parseTree of
        Just ("", value) -> Right . show . evaluate $ value
        _ -> Left "Syntax Error"

prompt :: IO ()
prompt = putStr "x> "
