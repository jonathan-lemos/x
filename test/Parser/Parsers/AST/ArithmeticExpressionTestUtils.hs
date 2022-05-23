module Parser.Parsers.AST.ArithmeticExpressionTestUtils where
import Data.Number.CReal
import State.XState
import Types.Evaluatable.Evaluatable
import Parser.Parser
import Parser.Error

mkState :: [(String, CReal)] -> XState
mkState = foldr (\(var, value) state -> putVar state var value) newState

eval :: (Evaluatable e) => XState -> Parser e -> (String -> Either ParseError (String, Either String CReal))
eval s = parse . fmap (`evaluate` s)

hasError :: String -> String -> Either ParseError a -> Bool
hasError msg ci (Left (ParseError a b)) = msg == a && ci == b
hasError _ _ _ = False
