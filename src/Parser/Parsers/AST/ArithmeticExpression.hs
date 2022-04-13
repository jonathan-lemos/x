module Parser.Parsers.AST.ArithmeticExpression where

import Control.Applicative
import Control.Monad
import Parser.Parser
import Parser.Parsers.Combinator.ParseWhile
import Parser.Parsers.Text.Char
import Types.AST.ArithmeticExpression
import Utils
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Numeric.XNumber

arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression =
    let operator =
            let mapOp '+' = Just Add
                mapOp '-' = Just Subtract
                mapOp _  = Nothing
            in mapOp <$?> char

        predicate = liftA2 (,) operator multiplication

    in liftA2 ArithmeticExpression multiplication (parseWhile predicate)

multiplication :: Parser Multiplication
multiplication =
    let operator =
            let mapOp '*' = Just Multiply
                mapOp '/' = Just Divide
                mapOp _   = Nothing
            in mapOp <$?> char

        predicate = liftA2 (,) operator power
        
    in liftA2 Multiplication power (parseWhile predicate)

power :: Parser Power
power = do
    f <- factor
    (Power f <$> (charEq '^' >> power)) <|> pure (NoPower f)

factor :: Parser Factor
factor =
    let parenExpr = do
            charEq '('
            e <- arithmeticExpression
            charEq ')'
            return e

    in (FactorNumber <$> xnumber) <|> (Parentheses <$> parenExpr)
