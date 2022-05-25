module Types.AST.ArithmeticExpression where

import Data.Number.CReal
import Data.List
import Data.Bifunctor
import Types.Evaluatable.Evaluatable
import State.XState
import Types.AST.Value.Value
import Utils.Either
import Types.AST.Value.Scalar


stringifyEvaluatable :: (Show a, Show b, Show c) => a -> [(b, c)] -> String
stringifyEvaluatable x xs =
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
    show (ArithmeticExpression x xs) = stringifyEvaluatable x xs

instance Evaluatable ArithmeticExpression where
    evaluate (ArithmeticExpression x xs) state =
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

instance Evaluatable Multiplication where
    evaluate (Multiplication x xs) state =
        let mapOp Multiply = (*)
            mapOp Divide   = (/)
            opList = first mapOp <$> xs
            in evaluateEvaluatable x state opList

data Power = Power Factor Power | NoPower Factor
    deriving Eq

instance Show Power where
    show (Power f p) = concat [show f, " ^ ", show p]
    show (NoPower f) = show f

instance Evaluatable Power where
    evaluate (Power f p) state = (**) <$> evaluate f state <*> evaluate p state
    evaluate (NoPower f) state = evaluate f state


data Factor = FactorValue Value | Parentheses ArithmeticExpression
    deriving Eq

instance Show Factor where
    show (FactorValue v) = show v
    show (Parentheses ae) = concat ["(", show ae, ")"]

instance Evaluatable Factor where
    evaluate (FactorValue (Value (Number n) _unit)) _ = Right n
    evaluate (FactorValue (Value (Variable v) _unit)) state = eitherFromMaybe ("Use of undeclared variable " <> show v) $ getVar state v
    evaluate (Parentheses p) state = evaluate p state
