module Parser.Parsers.AST.Value.UnitExpression where

import Types.AST.UnitExpression
import Parser.Parser
import Parser.Parsers.Combinator.Expression
import Parser.Parsers.Text.CharEq
import Parser.Parsers.AST.Value.UnitFactor
import Control.Applicative

unitMultExpr :: Parser UnitMultExpression
unitMultExpr =
    let ume x xs = UnitMultExpression x (fmap snd xs)
        in leftAssociativeExpression ume (charEq '*') unitFactor

unitExpr :: Parser UnitExpression
unitExpr = do
    ume1 <- unitMultExpr
    (UnitFraction ume1 <$> (charEq '/' >> unitMultExpr)) <|> pure (UnitProduct ume1)
