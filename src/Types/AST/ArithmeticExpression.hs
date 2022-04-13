module Types.AST.ArithmeticExpression where

import Types.XNumber

data AdditionOperator = Add | Subtract

data ArithmeticExpression = ArithmeticExpression Multiplication [(AdditionOperator, Multiplication)]

data MultiplicationOperator = Multiply | Divide

data Multiplication = Multiplication Power [(MultiplicationOperator, Power)]

data Power = Power Factor Power | NoPower Factor

data Factor = FactorNumber XNumber | Parentheses ArithmeticExpression