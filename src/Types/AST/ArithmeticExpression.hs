module Types.AST.ArithmeticExpression where

import Data.Number.CReal
import Control.Monad
import Data.List
import Data.Bifunctor
import Types.Expression


stringifyExpression :: (Show a, Show b, Show c) => a -> [(b, c)] -> String
stringifyExpression x xs =
    let toList (a, b) = [a, b]
        in unwords $ show x : ((toList . bimap show show) =<< xs)

evaluateExpression :: (Expression a) => a -> [(CReal -> CReal -> CReal, a)] -> CReal
evaluateExpression x =
    let apply a (f, b) = f a $ evaluate b
        in foldl' apply (evaluate x)


data AdditionOperator = Add | Subtract
    deriving Eq

instance Show AdditionOperator where
    show Add = "+"
    show Subtract = "-"


data ArithmeticExpression = ArithmeticExpression Multiplication [(AdditionOperator, Multiplication)]
    deriving Eq

instance Show ArithmeticExpression where
    show (ArithmeticExpression x xs) = stringifyExpression x xs

instance Expression ArithmeticExpression where
    evaluate (ArithmeticExpression x xs) =
        let mapOp Add      = (+)
            mapOp Subtract = (-)
            opList = first mapOp <$> xs
            in evaluateExpression x opList



data MultiplicationOperator = Multiply | Divide
    deriving Eq

instance Show MultiplicationOperator where
    show Multiply = "*"
    show Divide = "/"


data Multiplication = Multiplication Power [(MultiplicationOperator, Power)]
    deriving Eq

instance Show Multiplication where
    show (Multiplication x xs) = stringifyExpression x xs

instance Expression Multiplication where
    evaluate (Multiplication x xs) = 
        let mapOp Multiply = (*)
            mapOp Divide   = (/)
            opList = first mapOp <$> xs
            in evaluateExpression x opList

data Power = Power Factor Power | NoPower Factor
    deriving Eq

instance Show Power where
    show (Power f p) = concat [show f, " ^ ", show p]
    show (NoPower f) = show f

instance Expression Power where
    evaluate (Power f p) = evaluate f ** evaluate p
    evaluate (NoPower f) = evaluate f


data Factor = FactorNumber CReal | Parentheses ArithmeticExpression
    deriving Eq

instance Show Factor where
    show (FactorNumber r) = show r
    show (Parentheses ae) = concat ["(", show ae, ")"]

instance Expression Factor where
    evaluate (FactorNumber n) = n
    evaluate (Parentheses p)  = evaluate p 
