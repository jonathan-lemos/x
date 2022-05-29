module Types.AST.ArithmeticExpression where

import Data.Number.CReal
import Data.List
import Data.Bifunctor
import State.XState
import Utils.Either
import Types.AST.UnitExpression (UnitExpression)
import Types.AST.Token.Scalar
import Data.Maybe (fromMaybe)
import Evaluation.ToValue


stringifyLeftAssociativeExpression :: (Show a, Show b, Show c) => a -> [(b, c)] -> String
stringifyLeftAssociativeExpression x xs =
    let toList (a, b) = [a, b]
        in unwords $ show x : ((toList . bimap show show) =<< xs)

evaluateEvaluatable :: (Evaluatable a) => a -> XState -> [(CReal -> CReal -> CReal, a)] -> Either String CReal
evaluateEvaluatable x state =
    let apply a (f, b) = f <$> a <*> evaluate b state
        in foldl' apply (evaluate x state)


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
        let mapOp Add      = (+)
            mapOp Subtract = (-)
            opList = first mapOp <$> xs
            in evaluateEvaluatable x state opList


data MultiplicationOperator = Multiply | Divide
    deriving Eq

instance Show MultiplicationOperator where
    show Multiply = "*"
    show Divide = "/"


data Multiplication = Multiplication Power [(MultiplicationOperator, Power)]
    deriving Eq

instance Show Multiplication where
    show (Multiplication x xs) = stringifyEvaluatable x xs

instance ToValue Multiplication where
    toValue (Multiplication x xs) state =
        let mapOp Multiply = (*)
            mapOp Divide   = (/)
            opList = first mapOp <$> xs
            in evaluateEvaluatable x state opList

data Power = Power Factor Power | NoPower Factor
    deriving Eq

instance Show Power where
    show (Power f p) = concat [show f, " ^ ", show p]
    show (NoPower f) = show f

instance ToValue Power where
    toValue (Power f p) state = (**) <$> evaluate f state <*> evaluate p state
    toValue (NoPower f) state = evaluate f state


data Factor = Factor FactorQuantity (Maybe UnitExpression)
    deriving Eq

instance Show Factor where
    show (Factor quant unit) = show quant <> maybe "" ((" " <>) . show) unit

instance ToValue Factor where
    toValue (Factor quant unit) state =
        combineErrors (toValue quant) (<$> getUnit unit)


data FactorQuantity = FactorScalar Scalar | Parentheses ArithmeticExpression
    deriving Eq

instance Show FactorQuantity where
    show (FactorScalar sc) = show sc
    show (Parentheses ae) = "(" <> show ae <> ")"

instance ToValue FactorQuantity where
    toValue = undefined
