module Parser.Parsers.AST.ArithmeticExpression where

import Control.Applicative
import Control.Monad
import Data.Char (isDigit)
import Parser.Parser
import Parser.Parsers.Combinator.ManyMaybe
import Parser.Parsers.Combinator.Peek
import Parser.Parsers.Combinator.Precondition (precondition)
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.Text.Char
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Text.Whitespace
import Types.AST.ArithmeticExpression
import Utils.Monad
import Parser.Parsers.Combinator.LookaheadN (lookaheadN)
import Data.List

{- | Parses a left-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from left operator to right operator.

Logically, the type this returns should be a product of `subexpression` and `[operator, subexpression]`.

The `constructor` takes the first subexpression and a list of following (operator, subexpression) and returns the output value of the parser.

The `operator` parses an operator. Preceeding whitespace is parsed for you.

The `subexpressionParser` parses a subexpression. Preceeding whitespace is parsed for you.
-}
leftAssociativeExpression :: (sub -> [(op, sub)] -> expr) -> Parser op -> Parser sub -> Parser expr
leftAssociativeExpression constructor operator subexpressionParser =
    let predicates = manyMaybe $ precondition (whitespace >> operator) (whitespace >> subexpressionParser)
     in liftA2 constructor subexpressionParser predicates

{- | Parses a right-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from right operator to left operator.

Logically, the type this returns should be either a `subexpression` (base case) or a product of `subexpression`, `operator`, and `self` (recursive case).

The `noRight` constructor takes a subexpression and returns the (base case) output value of the parser.

The `right` constructor takes a subexpression, operator, and self, and returns the (recursive) output value of the parser.

The `operator` parses an operator. Preceeding whitespace is parsed for you.

The `subexpressionParser` parses a subexpression. Preceeding whitespace is parsed for you.
-}
rightAssociativeExpression :: (sub -> expr) -> (sub -> op -> expr -> expr) -> Parser op -> Parser sub -> Parser expr
rightAssociativeExpression noRight right operator subexpressionParser =
    let rhsParser = precondition (whitespace >> operator) (whitespace >> rightAssociativeExpression noRight right operator subexpressionParser)
     in do
            left <- subexpressionParser
            maybe (noRight left) (uncurry (right left)) <$> rhsParser

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
    let f x
          | "(" `isPrefixOf` x =
              do
                charEq '('
                whitespace
                e <- arithmeticExpression
                whitespace
                charEq ')'
                return $ Parentheses e

          | (length x >= 2 && head x `elem` "+-" && isDigit (x !! 1) ) ||
            (not (null x) && isDigit (head x)) =
                FactorNumber <$> creal

          | otherwise = fail "Expected a number or ( expression )"
     in whitespace >> lookaheadN 2 f
