module Parser.Parsers.Combinator.Expression where
import Parser.Parser
import Parser.Parsers.Combinator.ManyMaybe
import Parser.Parsers.Combinator.Precondition
import Control.Applicative

{- | Parses a left-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from left operator to right operator.

Logically, the type this returns should be a product of `subexpression` and `[operator, subexpression]`.

The `constructor` takes the first subexpression and a list of following (operator, subexpression) and returns the output value of the parser.

The `operator` parses an operator.

The `subexpressionParser` parses a subexpression.
-}
leftAssociativeExpression :: (sub -> [(op, sub)] -> expr) -> Parser op -> Parser sub -> Parser expr
leftAssociativeExpression constructor operator subexpressionParser =
    let predicates = manyMaybe $ precondition operator subexpressionParser
     in liftA2 constructor subexpressionParser predicates

{- | Parses a right-associative expression, which is `n >= 1` "subexpressions" joined by `n - 1` operators, processed from right operator to left operator.

Logically, the type this returns should be either a `subexpression` (base case) or a product of `subexpression`, `operator`, and `self` (recursive case).

The `noRight` constructor takes a subexpression and returns the (base case) output value of the parser.

The `right` constructor takes a subexpression, operator, and self, and returns the (recursive) output value of the parser.

The `operator` parses an operator.

The `subexpressionParser` parses a subexpression.
-}
rightAssociativeExpression :: (sub -> expr) -> (sub -> op -> expr -> expr) -> Parser op -> Parser sub -> Parser expr
rightAssociativeExpression noRight right operator subexpressionParser =
    let rhsParser = precondition operator (rightAssociativeExpression noRight right operator subexpressionParser)
     in do
            left <- subexpressionParser
            maybe (noRight left) (uncurry (right left)) <$> rhsParser
