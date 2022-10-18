module X.Control.Parser.Combinator.Expression where

import Control.Applicative
import X.Control.Parser
import X.Control.Parser.Combinator.Choice.LookaheadParse
import X.Control.Parser.Combinator.ManyMaybe
import X.Control.Parser.Combinator.Precondition
import X.Data.LeftAssociativeInfixChain
import X.Utils.LeftToRight

-- | Parses a left-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from left operator to right operator.
leftAssociativeExpression :: Parser val -> Parser op -> Parser (LeftAssociativeInfixChain op val)
leftAssociativeExpression subexpressionParser operatorParser =
    liftA2 fromHeadAndList subexpressionParser (manyMaybe $ precondition operatorParser subexpressionParser)

-- | Parses a right-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from right operator to left operator.
rightAssociativeExpression :: Parser val -> Parser op -> (val -> op -> val -> val) -> Parser val
rightAssociativeExpression subexpressionParser operatorParser combineValues =
    do
        head <- subexpressionParser
        next <-
            lookaheadParse
                [ operatorParser
                    >> pure
                        ( liftA2
                            ((,) ||@>|| Just)
                            operatorParser
                            (rightAssociativeExpression subexpressionParser operatorParser combineValues)
                        )
                , (pure . pure) Nothing
                ]

        return $ case next of
            Just (op, val) -> combineValues head op val
            Nothing -> head
