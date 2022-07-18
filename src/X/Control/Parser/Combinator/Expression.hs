module X.Control.Parser.Combinator.Expression where

import Data.Foldable
import X.Control.Parser
import X.Control.Parser.Combinator.ManyMaybe
import X.Control.Parser.Combinator.Precondition
import X.Data.Value
import X.Control.Parser.Combinator.Choice.LookaheadParse
import Control.Applicative
import X.Utils.LTR

-- | Parses a left-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from left operator to right operator.
leftAssociativeExpression :: Parser Value -> Parser op -> (Value -> op -> Value -> Value) -> Parser Value
leftAssociativeExpression subexpressionParser operatorParser combineValues =
    do
        head <- subexpressionParser
        subs <- manyMaybe $ precondition operatorParser subexpressionParser

        let combine value (op, sub) = combineValues value op sub
        return $ foldl' combine head subs

-- | Parses a right-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from right operator to left operator.
rightAssociativeExpression :: Parser Value -> Parser op -> (Value -> op -> Value -> Value) -> Parser Value
rightAssociativeExpression subexpressionParser operatorParser combineValues =
    do
        head <- subexpressionParser
        next <- lookaheadParse [
            operatorParser >> pure (
                liftA2
                ((,) ||@>|| Just)
                operatorParser
                (rightAssociativeExpression subexpressionParser operatorParser combineValues)
                ),
            (pure . pure) Nothing]

        return $ case next of
            Just (op, val) -> combineValues head op val
            Nothing -> head
