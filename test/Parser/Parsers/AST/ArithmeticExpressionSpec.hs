module Parser.Parsers.AST.ArithmeticExpressionSpec where

import Types.AST.ArithmeticExpression
import Test.Hspec
import Parser.Parsers.AST.ArithmeticExpression
import Parser.Parser
import Types.Evaluatable.Evaluatable
import Data.Bifunctor
import Data.Number.CReal
import Parser.Error
import Shell.State

mkState :: [(String, CReal)] -> XState
mkState = foldr (\(k, v) state -> putVar state k v) newState


eval :: (Evaluatable e) => XState -> Parser e -> Parser (Either String CReal)
eval state = fmap (`evaluate` state)

spec :: Spec
spec = do
    describe "factor tests" $ do
        let demoState = mkState [("a", 4),("b", 100)]
        let f = parse $ eval demoState factor

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

        it "evaluates variable" $ do
            f "a" `shouldBe` Right ("", 4)
            f "b" `shouldBe` Right ("", 100)

        it "errors on unknown variable" $ do
            f "noExist" `shouldBe` Left (ParseError "Use of ")

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
