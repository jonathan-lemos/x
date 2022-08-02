module X.Control.Parser.AST.ArithmeticExpression where

import Data.Char
import X.Control.Parser
import X.Control.Parser.Combinator.Branch.Conditional
import X.Control.Parser.Combinator.Choice.LookaheadParse
import X.Control.Parser.Combinator.Expression
import X.Control.Parser.Combinator.Possibly
import X.Control.Parser.Text.Char
import X.Control.Parser.Text.CharAny
import X.Control.Parser.Text.CharEq
import X.Control.Parser.Text.Whitespace
import X.Utils.Monad
import X.Data.Value
import X.Control.Parser.Numeric.CReal
import X.Control.Parser.AST.Token.Identifier
import X.Data.Operator
import X.Utils.LeftToRight

additiveExpression :: Parser Value
additiveExpression =
    let operator =
            let mapOp '+' = Just Add
                mapOp '-' = Just Sub
                mapOp _ = Nothing
             in mapOp <$?> char
    in leftAssociativeExpression (whitespace >> multiplicativeExpression) (whitespace >> operator) AdditiveChain

multiplicativeExpression :: Parser Value
multiplicativeExpression =
    let operator =
            let mapOp '*' = Just Mul
                mapOp '/' = Just Div
                mapOp _ = Nothing
             in mapOp <$?> char
     in leftAssociativeExpression (whitespace >> exponentiationExpression) (whitespace >> operator) MultiplicativeChain

exponentiationExpression :: Parser Value
exponentiationExpression =
    rightAssociativeExpression (whitespace >> factor) (whitespace >> charEq '^') $ \a _op b -> ExpChain a b

factor :: Parser Value
factor =
    let parenExpr = do
            charEq '('
            whitespace
            e <- additiveExpression
            whitespace
            charEq ')'
            return e
     in whitespace
            >> lookaheadParse
                [ charEq '(' >> pure parenExpr
                , (possibly (charAny "-+") >> conditional isDigit char) >> pure (creal |@>| Scalar)
                , conditional isAlpha char >> pure (identifier |@>| Variable)
                , fail "Expected a number, variable, or ( expression )"
                ]
