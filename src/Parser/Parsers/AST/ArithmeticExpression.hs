module Parser.Parsers.AST.ArithmeticExpression where

import Control.Applicative
import Control.Monad
import Parser.Parser
import Parser.Parsers.Combinator.ParseWhile
import Parser.Parsers.Text.Char
import Types.AST.ArithmeticExpression
import Utils.Monad
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.Text.Whitespace

leftAssociativeExpression :: (sub -> [(op, sub)] -> expr) -> Parser op -> Parser sub -> Parser expr
leftAssociativeExpression constructor operator subexpressionParser =
    let predicate = liftA2 (,) (whitespace >> operator) (whitespace >> subexpressionParser)
    in liftA2 constructor subexpressionParser (parseWhile predicate)

rightAssociativeExpression :: (sub -> expr) -> (sub -> op -> expr -> expr) -> Parser op -> Parser sub -> Parser expr
rightAssociativeExpression noRight right operator subexpressionParser =
    let rhsParser = liftA2 (,) (whitespace >> operator) (whitespace >> rightAssociativeExpression noRight right operator subexpressionParser)
    in do
        left <- subexpressionParser
        fmap (uncurry (right left)) rhsParser <|> pure (noRight left)


arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression =
    let operator =
            let mapOp '+' = Just Add
                mapOp '-' = Just Subtract
                mapOp _  = Nothing
            in mapOp <$?> char

    in leftAssociativeExpression ArithmeticExpression operator multiplication

multiplication :: Parser Multiplication
multiplication =
    let operator =
            let mapOp '*' = Just Multiply
                mapOp '/' = Just Divide
                mapOp _   = Nothing
            in mapOp <$?> char

    in leftAssociativeExpression Multiplication operator power

power :: Parser Power
power =
    let mkPower left _op = Power left
    in rightAssociativeExpression NoPower mkPower (charEq '^') factor

factor :: Parser Factor
factor =
    let parenExpr = do
            whitespace
            charEq '('
            whitespace
            e <- arithmeticExpression
            whitespace
            charEq ')'
            return e

    in (FactorNumber <$> (whitespace >> creal)) <|> (Parentheses <$> parenExpr)
