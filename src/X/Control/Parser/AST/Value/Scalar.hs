module X.Control.Parser.AST.Value.Scalar where

import X.Control.Parser
import X.Control.Parser.Combinator.Choice.LookaheadParse
import X.Control.Parser.Text.CharAny
import X.Control.Parser.Combinator.Branch.Conditional
import Data.Char
import X.Control.Parser.Text.Char
import X.Control.Parser.Numeric.CReal
import X.Control.Parser.AST.Token.Identifier
import X.Control.Parser.Combinator.Possibly
import X.Data.AST.Token.Scalar

-- | Matches a scalar, which is an identifier or number (without unit)
scalar :: Parser Scalar
scalar = lookaheadParse
            [ possibly (charAny "+-") >> conditional isDigit char >> pure (Number <$> creal)
            , conditional isAlpha char >> pure (Variable <$> identifier)
            , fail "Expected a number or a variable name"
            ]
