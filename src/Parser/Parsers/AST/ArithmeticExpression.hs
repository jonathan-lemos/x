module Parser.Parsers.AST.ArithmeticExpression where

import Control.Applicative
import Control.Monad ()
import Data.Char (isDigit, isAlpha)
import Parser.Parser
import Parser.Parsers.Text.Char
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Text.Whitespace
import Types.AST.ArithmeticExpression
import Utils.Monad
import Parser.Parsers.Combinator.Choice.LookaheadParse
import Parser.Parsers.Combinator.Branch.Conditional
import Parser.Parsers.Combinator.Possibly
import Parser.Parsers.Text.CharAny
import Parser.Parsers.Combinator.Expression

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
     in leftAssociativeExpression ArithmeticExpression operator multiplication

multiplication :: Parser Multiplication
multiplication =
    let operator =
            let mapOp '*' = Just Multiply
                mapOp '/' = Just Divide
                mapOp _ = Nothing
             in mapOp <$?> char
     in leftAssociativeExpression Multiplication operator power

power :: Parser Power
power =
    let mkPower left _op = Power left
     in rightAssociativeExpression NoPower mkPower (charEq '^') factor

factor :: Parser Factor
factor =
    let parenExpr = do
                charEq '('
                whitespace
                e <- arithmeticExpression
                whitespace
                charEq ')'
                return $ Parentheses e
    in
    whitespace >>
        lookaheadParse
            [
                charEq '(' >> pure parenExpr,
                ((possibly (charAny "-+") >> conditional isDigit char) <|> conditional isAlpha char) >> pure (FactorValue <$> value),
                fail "Expected a number, variable, or ( expression )"
            ]
