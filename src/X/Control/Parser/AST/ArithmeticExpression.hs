module X.Control.Parser.AST.ArithmeticExpression where

import Data.Char
import qualified Data.List.NonEmpty as NE
import X.Control.Parser
import X.Control.Parser.AST.Token.Identifier
import X.Control.Parser.Combinator.Branch.Conditional
import X.Control.Parser.Combinator.Choice.LookaheadParse
import X.Control.Parser.Combinator.Expression
import X.Control.Parser.Combinator.Possibly
import X.Control.Parser.Numeric.CReal
import X.Control.Parser.Text.Char
import X.Control.Parser.Text.CharAny
import X.Control.Parser.Text.CharEq
import X.Control.Parser.Text.Whitespace
import X.Data.AST.Arithmetic
import X.Data.Operator
import X.Utils.LeftToRight
import X.Utils.Monad

additiveExpression :: Parser AdditiveChain
additiveExpression =
    let operator =
            let mapOp '+' = Just Add
                mapOp '-' = Just Sub
                mapOp _ = Nothing
             in mapOp <$?> char
     in leftAssociativeExpression (whitespace >> multiplicativeExpression) (whitespace >> operator)
            |@>| AdditiveChain

multiplicativeExpression :: Parser MultiplicativeChain
multiplicativeExpression =
    let operator =
            let mapOp '*' = Just Mul
                mapOp '/' = Just Div
                mapOp _ = Nothing
             in mapOp <$?> char
     in leftAssociativeExpression (whitespace >> exponentiationExpression) (whitespace >> operator)
            |@>| MultiplicativeChain

exponentiationExpression :: Parser ExpChain
exponentiationExpression =
    rightAssociativeExpression (whitespace >> factor |@>| NE.singleton) (whitespace >> charEq '^') (\a _op b -> a <> b)
        |@>| Exponentiation

factor :: Parser Factor
factor =
    let parenExpr = do
            charEq '('
            whitespace
            e <- additiveExpression
            whitespace
            charEq ')'
            return Parentheses e
     in whitespace
            >> lookaheadParse
                [ charEq '(' >> pure parenExpr
                , (possibly (charAny "-+") >> conditional isDigit char) >> pure (creal |@>| Scalar)
                , conditional isAlpha char >> pure (identifier |@>| Variable)
                , fail "Expected a number, variable, or ( expression )"
                ]
