module Parser.Parsers.AST.ArithmeticExpressionSpec where

import Types.AST.ArithmeticExpression
import Test.Hspec
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parser
import Types.Expression
import Data.Bifunctor
import Data.Number.CReal
import Parser.Error (ParseError(ParseError))


eval :: (Expression e) => Parser e -> Parser CReal
eval = fmap evaluate

spec :: Spec
spec = do
    describe "factor tests" $ do
        let f = parse $ eval factor

        it "evaluates a single number" $ do
            f "2" `shouldBe` Right ("", 2)

        it "evaluates a single number with whitespace" $ do
            f " 2" `shouldBe` Right ("", 2)

        it "evaluates a parenthesized number" $ do
            f "(2)" `shouldBe` Right ("", 2)

        it "evaluates a parenthesized number w/ whitespace" $ do
            f " ( 2 )" `shouldBe` Right ("", 2)

        it "gives expected error message on unknown char" $ do
            f "@" `shouldBe` Left (ParseError "Expected a number or ( expression )" "@")

    describe "evaluates expressions properly" $ do
        let ae = parse $ eval arithmeticExpression

        it "evaluates a single number" $ do
            ae "2" `shouldBe` Right ("", 2)

        it "evaluates a basic power expression" $ do
            ae "3^2" `shouldBe` Right ("", 9)

        it "evaluates a basic multiplication expression" $ do
            ae "3*2" `shouldBe` Right ("", 6)

        it "evaluates a basic addition expression" $ do
            ae "2+2" `shouldBe` Right ("", 4)

        it "evaluates multi power expression" $ do
            ae "4^3^2" `shouldBe` Right ("", 262144)

        it "evaluates multi divide expression" $ do
            ae "100/2/5" `shouldBe` Right ("", 10)

        it "evaluates multi subtract expression" $ do
            ae "3-2-1" `shouldBe` Right ("", 0)

        it "evaluates expression with parentheses" $ do
            ae "2+(5*10)" `shouldBe` Right ("", 52)
            ae "(2+2)" `shouldBe` Right ("", 4)

        it "evaluates complex expression" $ do
            ae "2+3*4^(1/2)" `shouldBe` Right ("", 8)

        it "evaluates expression with whitespace" $ do
            ae "2 + 3 * 4 ^ ( 1 / 2 )" `shouldBe` Right ("", 8)

        it "stops evaluating when expression is done" $ do
            ae "2+2 foobar" `shouldBe` Right (" foobar", 4)

        it "parses nothing on invalid expression" $ do
            ae "+" `shouldBe` Left (ParseError "Expected a number or ( expression )" "+")
            ae "(2+2" `shouldBe` Left (ParseError "Expected ')'" "")
            ae "2*" `shouldBe` Left (ParseError "Expected a number or ( expression )" "")
