module Parser.Parsers.AST.Value.Value where

import Parser.Parsers.Combinator.FirstThatParses
import Types.Value.Value
import Parser.Parser
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.AST.Token.Identifier
import Control.Applicative

value :: Parser Value
value = undefined
{-
    firstThatParses
        [ XNumber <$> creal
        , XVariable <$> identifier
        ]
        "Expected a number or a variable name"
        -}