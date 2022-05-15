module Parser.Parsers.AST.Value.XValue where

import Parser.Parsers.Combinator.FirstThatParses
import Types.XValue
import Parser.Parser
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.AST.Token.Identifier
import Control.Applicative

xvalue :: Parser XValue
xvalue =
    firstThatParses
        [ XNumber <$> creal
        , XVariable <$> identifier
        ]
        "Expected a number or a variable name"