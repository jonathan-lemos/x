module Parser.Parsers.AST.Value.UnitFactor where

import Parser.Parser
import Parser.Parsers.AST.Token.Identifier
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.Text.CharEq
import Types.AST.UnitExpression
import Parser.Parsers.Combinator.Choice.LookaheadParse

unitFactor :: Parser UnitFactor
unitFactor = do
    u <- identifier
    lookaheadParse
        [ charEq '^' >> pure (UnitPower u <$> (charEq '^' >> creal))
        , pure . pure $ JustUnit u
        ]