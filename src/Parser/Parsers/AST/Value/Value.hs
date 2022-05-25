module Parser.Parsers.AST.Value.Value where

import Control.Applicative
import Control.Monad
import Data.Char
import Parser.Parser
import Parser.Parsers.AST.Token.Identifier
import Parser.Parsers.AST.Value.UnitExpression
import Parser.Parsers.Combinator.Branch.Conditional
import Parser.Parsers.Combinator.Choice.LookaheadParse
import Parser.Parsers.Combinator.Possibly
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.Text.Char
import Parser.Parsers.Text.CharAny
import Parser.Parsers.Text.Eof
import Parser.Parsers.Text.Whitespace
import Types.AST.Value.Scalar
import Types.AST.Value.Value

value :: Parser Value
value = do
    scalar <-
        lookaheadParse
            [ possibly (charAny "+-") >> conditional isDigit char >> pure (Number <$> creal)
            , conditional isAlpha char >> pure (Variable <$> identifier)
            , fail "Expected a number or a variable name"
            ]

    lookaheadParse
        [ whitespace >> identifier >> (void (charAny "*^") <|> eof <|> void (some (conditional isSpace char)))
            >> pure (whitespace >> (Value scalar . Just <$> unitExpr))
        , whitespace >> identifier >> charAny "*^"
            >> pure (fail "Terms of a unit expression cannot have spaces between them")
        , pure . pure $ Value scalar Nothing
        ]
