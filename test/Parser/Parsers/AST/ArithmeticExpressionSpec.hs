module Parser.Parsers.AST.ArithmeticExpressionSpec where

import Data.Bifunctor
import Data.Number.CReal
import Parser.Error
import Parser.Parser
import Parser.Parsers.AST.ArithmeticExpression
import State.XState
import Test.Hspec
import TestUtils.ArithmeticExpression
import TestUtils.Parser
import Types.AST.ArithmeticExpression
import Types.AST.UnitExpression
import Types.AST.Value.Scalar
import Types.AST.Value.Value
import Types.Evaluatable.Evaluatable

isParentheses :: Factor -> Bool
isParentheses (Parentheses _) = True
isParentheses _ = False

spec :: Spec
spec = do
    passPartialFailFnSpec
        "factor"
        factor
        [ ("2", (== FactorValue (Value (Number 2) Nothing)))
        , ("2 kg", (== FactorValue (Value (Number 2) (Just . UnitProduct $ UnitMultExpression (JustUnit "kg") []))))
        , ("(2)", isParentheses)
        , ("( 2 )", isParentheses)
        , ("(2 + 2) kg", isParentheses)
        , ("a", (== FactorValue (Value (Variable "a") Nothing)))
        , ("foobar", (== FactorValue (Value (Variable "foobar") Nothing)))
        ]
        [ ("2 || 5", (== FactorValue (Value (Number 2) Nothing)), " || 5")
        , ("(2) || 5", isParentheses, " || 5")
        ]
        [("@", "Expected a number, variable, or ( expression )", "@")]


    let state = mkState [("a", 4), ("foo", 9)]
    let ae = (`evaluate` state) <$> arithmeticExpression

    let evaluatesTo n (Right r) = n == r
        evaluatesTo _ _ = False

    let evalErrorsWith msg (Left m) = msg == m
        evalErrorsWith _ _ = False

    passPartialFailFnSpec "arithmetic expression evaluation" ae
        [("2", evaluatesTo 2)
        ,("3^2", evaluatesTo 9)
        ,("2^3^2", evaluatesTo 512)
        ,("2+3",evaluatesTo 5)
        ,("3-2-1",evaluatesTo 0)
        ,("100/2/5", evaluatesTo 10)
        ,("3*2", evaluatesTo 6)
        ,("2+(5*10)", evaluatesTo 52)
        ,("(2+5)*10", evaluatesTo 70)
        ,("(2+2)", evaluatesTo 4)
        ,("2+3*4^(1/2)", evaluatesTo 8)
        ,("2 + 3 * 4 ^ ( 1 / 2 )", evaluatesTo 8)]
        [("2 + 2 || 5 * 7", evaluatesTo 4, " || 5 * 7")
        ,("(5*8),(7*4)", evaluatesTo 40, ",(7*4)")
        ,"2 foobar", evaluatesTo ]
        []

    describe "evaluates expressions properly" $ do

        it "stops evaluating when expression is done" $ do
            ae "2+2 foobar" `shouldBe` Right (" foobar", Right 4)

        it "parses nothing on invalid expression" $ do
            ae "+" `shouldBe` Left (ParseError "Expected a number, variable, or ( expression )" "+")
            ae "(2+2" `shouldBe` Left (ParseError "Expected ')'" "")
            ae "2*" `shouldBe` Left (ParseError "Expected a number, variable, or ( expression )" "")

        it "correctly parses variable" $ do
            ae "a" `shouldBe` Right ("", Right 4)
            ae "a + 4" `shouldBe` Right ("", Right 8)
            ae "a + foo" `shouldBe` Right ("", Right 13)

        it "errors on invalid variable" $ do
            ae "bar" `shouldBe` Right ("", Left "Use of undeclared variable \"bar\"")
