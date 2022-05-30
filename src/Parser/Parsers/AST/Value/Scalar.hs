module Parser.Parsers.AST.Value.Scalar where

import Parser.Parser
import Parser.Parsers.Combinator.Choice.LookaheadParse
import Parser.Parsers.Text.CharAny
import Parser.Parsers.Combinator.Branch.Conditional
import Data.Char
import Parser.Parsers.Text.Char
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.AST.Token.Identifier
import Parser.Parsers.Combinator.Possibly
import Types.AST.Token.Scalar

scalar :: Parser Scalar
scalar = lookaheadParse
            [ possibly (charAny "+-") >> conditional isDigit char >> pure (Number <$> creal)
            , conditional isAlpha char >> pure (Variable <$> identifier)
            , fail "Expected a number or a variable name"
            ]
