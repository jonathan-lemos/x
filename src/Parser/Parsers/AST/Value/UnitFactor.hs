module Parser.Parsers.AST.Value.UnitFactor where

import Types.AST.UnitExpression
import Parser.Parser
import Parser.Parsers.Text.CharEq
import Parser.Parsers.Numeric.CReal
import Parser.Parsers.Text.Whitespace
import Parser.Parsers.AST.Token.Identifier
import Control.Applicative

unitFactor :: Parser UnitFactor
unitFactor =
    let suffix = do
            charEq '^'
            creal
    in do
        whitespace
        u <- identifier
        UnitPower u <$> suffix <|> pure (JustUnit u)