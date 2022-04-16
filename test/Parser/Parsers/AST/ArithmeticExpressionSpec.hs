module Parser.Parsers.AST.ArithmeticExpressionSpec where

import Types.AST.ArithmeticExpression
import Test.Hspec
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parser
import Types.Expression
import Data.Bifunctor
import Data.Number.CReal


eval :: (Expression e) => Parser e -> Parser CReal
eval = fmap evaluate

spec :: Spec
spec = do
    describe "evaluates expressions properly" $ do
        let ae = parse $ eval arithmeticExpression

        it "evaluates a single number" $ do
            ae "2" `shouldBe` Just ("", 2)

        it "evaluates a basic power expression" $ do
            ae "3^2" `shouldBe` Just ("", 9)

        it "evaluates a basic multiplication expression" $ do
            ae "3*2" `shouldBe` Just ("", 6)

        it "evaluates a basic addition expression" $ do
            ae "2+2" `shouldBe` Just ("", 4)

        it "evaluates multi power expression" $ do
            ae "4^3^2" `shouldBe` Just ("", 262144)

        it "evaluates multi divide expression" $ do
            ae "100/2/5" `shouldBe` Just ("", 10)

        it "evaluates multi subtract expression" $ do
            ae "3-2-1" `shouldBe` Just ("", 0)

        it "evaluates expression with parentheses" $ do
            ae "2+(5*10)" `shouldBe` Just ("", 52)
            ae "(2+2)" `shouldBe` Just ("", 4)

        it "evaluates complex expression" $ do
            ae "2+3*4^(1/2)" `shouldBe` Just ("", 8)

        it "evaluates expression with whitespace" $ do
            ae "2 + 3 * 4 ^ ( 1 / 2 )" `shouldBe` Just ("", 8)

        it "stops evaluating when expression is done" $ do
            ae "2+2 foobar" `shouldBe` Just (" foobar", 4)

        it "parses nothing on invalid expression" $ do
            ae "+" `shouldBe` Nothing
            ae "(2+2" `shouldBe` Nothing
