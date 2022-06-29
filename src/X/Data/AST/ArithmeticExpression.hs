module X.Data.AST.ArithmeticExpression where

import Data.Bifunctor
import Data.Foldable
import X.Evaluation.Arithmetic
import X.Evaluation.ToValue
import X.Data.State.Value
import X.Data.State.XState
import X.Data.AST.Token.Scalar
import X.Data.AST.UnitExpression
import X.Utils.String
import X.Control.Try
import Control.Applicative

-- | Turns a list of expressions (a:c) joined by operators (b) into a string
stringifyLeftAssociativeExpression :: (Show a, Show b, Show c) => a -> [(b, c)] -> String
stringifyLeftAssociativeExpression x xs =
    let toList (a, b) = [a, b]
     in unwords $ show x : ((toList . bimap show show) =<< xs)

-- | Evaluates expressions (a) joined by operators
--
-- An operator function takes a `Value`, `Expression`, and `State`, and returns either an error message or a new `Value`.
evaluateLeftAssociativeExpression :: (ToValue a) => a -> XState -> [(Value -> a -> XState -> Try Value, a)] -> Try Value
evaluateLeftAssociativeExpression x state =
    foldl' (\acc (f, v) -> acc >>= \av -> f av v state) (toValue x state)

data AdditionOperator = Add | Subtract
    deriving (Eq)

instance Show AdditionOperator where
    show Add = "+"
    show Subtract = "-"

-- | An addition expression. `-` and `+` are handled at this level.
data ArithmeticExpression = ArithmeticExpression Multiplication [(AdditionOperator, Multiplication)]
    deriving (Eq)

instance Show ArithmeticExpression where
    show (ArithmeticExpression x xs) = stringifyLeftAssociativeExpression x xs

instance ToValue ArithmeticExpression where
    toValue (ArithmeticExpression x xs) state =
        let mapOp Add = addValues
            mapOp Subtract = subValues
            opList = first mapOp <$> xs
         in evaluateLeftAssociativeExpression x state opList

data MultiplicationOperator = Multiply | Divide
    deriving (Eq)

instance Show MultiplicationOperator where
    show Multiply = "*"
    show Divide = "/"

-- | A multiplication expression. `*` and `/` are handled at this level.
data Multiplication = Multiplication Power [(MultiplicationOperator, Power)]
    deriving (Eq)

instance Show Multiplication where
    show (Multiplication x xs) = stringifyLeftAssociativeExpression x xs

instance ToValue Multiplication where
    toValue (Multiplication x xs) state =
        let mapOp Multiply = multValues
            mapOp Divide = divValues
            opList = first mapOp <$> xs
         in evaluateLeftAssociativeExpression x state opList

-- | A power expression. `^` is handled at this level.
data Power = Power UnitQuantity Power | NoPower UnitQuantity
    deriving (Eq)

instance Show Power where
    show (Power f p) = concat [show f, " ^ ", show p]
    show (NoPower f) = show f

instance ToValue Power where
    toValue (Power f p) state = liftA2 (,) (toValue f state) (toValue p state) >>= ($ state) . uncurry expValues
    toValue (NoPower f) state = toValue f state

-- | A quantity or parentheses expression that may or may not have a unit.
data UnitQuantity = UnitQuantity Factor (Maybe UnitExpression)
    deriving (Eq)

instance Show UnitQuantity where
    show (UnitQuantity quant unit) = show quant <> maybe "" ((" " <>) . show) unit

instance ToValue UnitQuantity where
    toValue (UnitQuantity quant unitExpr) state =
        let unit = sequenceA $ (`ueToUnit` state) <$> unitExpr
            quantValue = toValue quant state
         in liftA2 (,) quantValue unit
                >>= \(q, u) ->
                    case (q, u) of
                        (Numeric qReal (Just qUnit), Nothing) -> Success $ Numeric qReal (Just qUnit)
                        (Numeric qReal Nothing, Just unit) -> Success $ Numeric qReal (Just unit)
                        (Numeric qReal Nothing, Nothing) -> Success $ Numeric qReal Nothing
                        (Numeric qReal (Just qUnit), Just unit) ->
                            if qUnit == unit
                                then Success $ Numeric qReal (Just qUnit)
                                else fail $ "Cannot add unit " <> parenthesize (show unit) <> " to a variable that already has a unit " <> parenthesize (show qUnit)

-- | A scalar or parentheses expression.
data Factor = FactorScalar Scalar | Parentheses ArithmeticExpression
    deriving (Eq)

instance Show Factor where
    show (FactorScalar sc) = show sc
    show (Parentheses ae) = parenthesize $ show ae

instance ToValue Factor where
    toValue (FactorScalar sc) = toValue sc
    toValue (Parentheses ae) = toValue ae
