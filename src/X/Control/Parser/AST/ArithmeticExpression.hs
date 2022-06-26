module X.Control.Parser.AST.ArithmeticExpression where

import Control.Applicative
import Data.Char (isAlpha, isDigit)
import X.Control.Parser
import X.Control.Parser.AST.Value.Scalar
import X.Control.Parser.AST.Value.UnitExpression
import X.Control.Parser.Combinator.Branch.Conditional
import X.Control.Parser.Combinator.Choice.LookaheadParse
import X.Control.Parser.Combinator.Expression
import X.Control.Parser.Combinator.Possibly
import X.Control.Parser.Text.Char
import X.Control.Parser.Text.CharAny
import X.Control.Parser.Text.CharEq
import X.Control.Parser.Text.Whitespace
import X.Data.AST.ArithmeticExpression
import X.Utils.Monad

{- | Parses an arithmetic expression, which is some combination of addition, subtraction, multiplication, division, exponentiation, and parentheses.

This parser does not evaluate the expression; it only returns a parse tree.

This parser specifically handles addition and subtraction (if present). The other parsers in this module handle multiplication, exponentiation, and parentheses.

This series of parsers can be represented with the following context-free grammar:

```
arithmeticExpression -> arithmeticExpression + multiplication | arithmeticExpression - multiplication | multiplication
multiplication       -> multiplication * power | multiplication / power | power
power                -> factor ^ power | factor
factor               -> creal | ( arithmeticExpression )
```
-}
arithmeticExpression :: Parser ArithmeticExpression
arithmeticExpression =
    let operator =
            let mapOp '+' = Just Add
                mapOp '-' = Just Subtract
                mapOp _ = Nothing
             in mapOp <$?> char
     in leftAssociativeExpression ArithmeticExpression (whitespace >> operator) (whitespace >> multiplication)

multiplication :: Parser Multiplication
multiplication =
    let operator =
            let mapOp '*' = Just Multiply
                mapOp '/' = Just Divide
                mapOp _ = Nothing
             in mapOp <$?> char
     in leftAssociativeExpression Multiplication (whitespace >> operator) (whitespace >> power)

power :: Parser Power
power =
    let mkPower left _op = Power left
     in rightAssociativeExpression NoPower mkPower (whitespace >> charEq '^') (whitespace >> unitQuantity)

unitQuantity :: Parser UnitQuantity
unitQuantity =
    do
        fact <- factor
        UnitQuantity fact . Just <$> (whitespace >> unitExpr)
            <|> pure (UnitQuantity fact Nothing)
            <|> fail "Expected [number] [unit]?"

factor :: Parser Factor
factor =
    let parenExpr = do
            charEq '('
            whitespace
            e <- arithmeticExpression
            whitespace
            charEq ')'
            return $ Parentheses e
     in whitespace
            >> lookaheadParse
                [ charEq '(' >> pure parenExpr
                , ((possibly (charAny "-+") >> conditional isDigit char) <|> conditional isAlpha char) >> pure (FactorScalar <$> scalar)
                , fail "Expected a number, variable, or ( expression )"
                ]
