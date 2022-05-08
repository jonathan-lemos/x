module Parser.Parsers.AST.Statement (statement) where

import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression (arithmeticExpression)
import Parser.Parsers.AST.Primitive.Identifier
import Parser.Parsers.Combinator.FirstThatParses
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Text.Whitespace
import Types.AST.Assignment
import Types.AST.Statement

assignmentWithLeadingVariable :: String -> Parser Statement
assignmentWithLeadingVariable v = do
    charEq '='
    whitespace
    StmtAssignment . Assignment v <$> arithmeticExpression

statementWithLeadingVariable :: Parser Statement
statementWithLeadingVariable =
    let variableSuffix v =
            firstThatParses
                [assignmentWithLeadingVariable v, StmtExpr <$> arithmeticExpression]
                "Expected 'var = expr' or 'expr'"
     in identifier >>= variableSuffix

statement :: Parser Statement
statement =
    whitespace
        >> firstThatParses
            [statementWithLeadingVariable, StmtExpr <$> arithmeticExpression]
            "Expected 'var = expr' or 'expr'"
