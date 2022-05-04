module Parser.Parsers.AST.XValue where

import Parser.Parsers.Combinator.FirstThatParses
import Types.XValue

identifier :: Parser String
identifier = some (char isAlpha)

xvalue :: Parser XValue
xvalue =
    firstThatParses
        [ XNumber <$> creal
        , XVariable <$> identifier
        ]
        <|> fail "Expected a number or a variable name"