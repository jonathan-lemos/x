module Types.AST.ArithmeticExpression where

import Data.Bifunctor
import State.XState
import Utils.Either
import Types.AST.UnitExpression (UnitExpression)
import Types.AST.Token.Scalar
import Evaluation.ToValue
import State.Value
import Evaluation.Arithmetic
import Data.Foldable


stringifyLeftAssociativeExpression :: (Show a, Show b, Show c) => a -> [(b, c)] -> String
stringifyLeftAssociativeExpression x xs =
    let toList (a, b) = [a, b]
        in unwords $ show x : ((toList . bimap show show) =<< xs)

evaluateLeftAssociativeExpression :: (ToValue a) => a -> XState -> [(Value -> a -> XState -> Either String Value, a)] -> Either String Value
evaluateLeftAssociativeExpression x state =
    foldl' (\acc (f, v) -> acc >>= \av -> f av v state ) (toValue x state)


data AdditionOperator = Add | Subtract
    deriving Eq

instance Show AdditionOperator where
    show Add = "+"
    show Subtract = "-"


data ArithmeticExpression = ArithmeticExpression Multiplication [(AdditionOperator, Multiplication)]
    deriving Eq

instance Show ArithmeticExpression where
    show (ArithmeticExpression x xs) = stringifyLeftAssociativeExpression x xs

instance ToValue ArithmeticExpression where
    toValue (ArithmeticExpression x xs) state =
        let mapOp Add      = addValues
            mapOp Subtract = subValues
            opList = first mapOp <$> xs
            in evaluateLeftAssociativeExpression x state opList


data MultiplicationOperator = Multiply | Divide
    deriving Eq

instance Show MultiplicationOperator where
    show Multiply = "*"
    show Divide = "/"


data Multiplication = Multiplication Power [(MultiplicationOperator, Power)]
    deriving Eq

instance Show Multiplication where
    show (Multiplication x xs) = stringifyLeftAssociativeExpression x xs

instance ToValue Multiplication where
    toValue (Multiplication x xs) state =
        let mapOp Multiply = multValues
            mapOp Divide   = divValues
            opList = first mapOp <$> xs
            in evaluateLeftAssociativeExpression x state opList

data Power = Power UnitQuantity Power | NoPower UnitQuantity
    deriving Eq

instance Show Power where
    show (Power f p) = concat [show f, " ^ ", show p]
    show (NoPower f) = show f

instance ToValue Power where
    toValue (Power f p) state = combineErrors (toValue f state) (toValue p state) >>= ($ state) . uncurry expValues
    toValue (NoPower f) state = toValue f state


data UnitQuantity = UnitQuantity Factor (Maybe UnitExpression)
    deriving Eq

instance Show UnitQuantity where
    show (UnitQuantity quant unit) = show quant <> maybe "" ((" " <>) . show) unit

instance ToValue UnitQuantity where
    toValue (UnitQuantity quant unit) state =
        combineErrors (toValue quant state) (<$> getUnit unit)


data Factor = FactorScalar Scalar | Parentheses ArithmeticExpression
    deriving Eq

instance Show Factor where
    show (FactorScalar sc) = show sc
    show (Parentheses ae) = "(" <> show ae <> ")"

instance ToValue Factor where
    toValue = undefined
