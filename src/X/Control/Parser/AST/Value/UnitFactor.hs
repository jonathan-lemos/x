module X.Control.Parser.AST.Value.UnitFactor where

import X.Control.Parser
import X.Control.Parser.AST.Token.Identifier
import X.Control.Parser.Numeric.CReal
import X.Control.Parser.Text.CharEq
import X.Data.AST.UnitExpression
import X.Control.Parser.Combinator.Choice.LookaheadParse

unitFactor :: Parser UnitFactor
unitFactor = do
    u <- identifier
    lookaheadParse
        [ charEq '^' >> pure (UnitPower u <$> (charEq '^' >> creal))
        , pure . pure $ JustUnit u
        ]