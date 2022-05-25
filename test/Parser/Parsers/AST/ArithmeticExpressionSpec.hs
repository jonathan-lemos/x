module Parser.Parsers.AST.ArithmeticExpressionSpec where

import Types.AST.ArithmeticExpression
import Test.Hspec
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parser
import Data.Bifunctor
import Data.Number.CReal
import Parser.Error
import Types.AST.Value.Value
import Types.Evaluatable.Evaluatable
import State.XState
import TestUtils.ArithmeticExpression
import Types.AST.Value.Scalar

isParentheses :: Either ParseError (String, Factor) -> Bool
isParentheses (Right (_, Parentheses _)) = True
isParentheses _ = False

spec :: Spec
spec = do
    describe "factor parses correctly" $ do
        let f = parse factor

        it "evaluates a single number" $ do
            f "2" `shouldBe` Right ("", FactorValue (Value (Number 2) Nothing))

        it "evaluates a single number with whitespace" $ do
            f " 2" `shouldBe` Right ("", FactorValue (Value (Number 2) Nothing))

        it "evaluates a parenthesized number" $ do
            f "(2)" `shouldSatisfy` isParentheses

        it "evaluates a parenthesized number w/ whitespace" $ do
            f " ( 2 )" `shouldSatisfy` isParentheses

        it "gives expected error message on unknown char" $ do
            f "@" `shouldBe` Left (ParseError "Expected a number, variable, or ( expression )" "@")

        it "evaluates a variable" $ do
            f "a" `shouldBe` Right ("", FactorValue (Value (Variable "a") Nothing))
            f "foobar" `shouldBe` Right ("", FactorValue (Value (Variable "foobar") Nothing))

    describe "evaluates expressions properly" $ do
        let state = mkState [("a", 4), ("foo", 9)]
        let ae = eval state arithmeticExpression

        it "evaluates a single number" $ do
            ae "2" `shouldBe` Right ("", Right 2)

        it "evaluates a basic power expression" $ do
            ae "3^2" `shouldBe` Right ("", Right 9)

        it "evaluates a basic multiplication expression" $ do
            ae "3*2" `shouldBe` Right ("", Right 6)

        it "evaluates a basic addition expression" $ do
            ae "2+2" `shouldBe` Right ("", Right 4)

        it "evaluates multi power expression" $ do
            ae "4^3^2" `shouldBe` Right ("", Right 262144)

        it "evaluates multi divide expression" $ do
            ae "100/2/5" `shouldBe` Right ("", Right 10)

        it "evaluates multi subtract expression" $ do
            ae "3-2-1" `shouldBe` Right ("", Right 0)

        it "evaluates expression with parentheses" $ do
            ae "2+(5*10)" `shouldBe` Right ("", Right 52)
            ae "(2+2)" `shouldBe` Right ("", Right 4)

        it "evaluates complex expression" $ do
            ae "2+3*4^(1/2)" `shouldBe` Right ("", Right 8)

        it "evaluates expression with whitespace" $ do
            ae "2 + 3 * 4 ^ ( 1 / 2 )" `shouldBe` Right ("", Right 8)

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
