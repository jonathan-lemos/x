module X.Control.Parser.AST.Value.UnitExpression where

import X.Data.AST.UnitExpression
import X.Control.Parser
import X.Control.Parser.Combinator.Expression
import X.Control.Parser.Text.CharEq
import X.Control.Parser.AST.Value.UnitFactor
import Control.Applicative

-- | Matches a chain of unit * unit * ...
unitMultExpr :: Parser UnitMultExpression
unitMultExpr =
    let ume x xs = UnitMultExpression x (fmap snd xs)
        in leftAssociativeExpression ume (charEq '*') unitFactor

-- | Matches unitMult / unitMult or unitMult
unitExpr :: Parser UnitExpression
unitExpr = do
    ume1 <- unitMultExpr
    (UnitFraction ume1 <$> (charEq '/' >> unitMultExpr)) <|> pure (UnitProduct ume1)
