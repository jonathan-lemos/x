module Parser.Parsers.Text.ConditionalSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Text.Char
import Parser.Parsers.Combinator.Conditional

spec :: Spec
spec = do
    describe "Conditional parser" $ do
        let f = parse $ conditional char (== 'a')

        it "reads char if condition is met" $ do
            f "abc" `shouldBe` Just ("bc", 'a')
            f "a" `shouldBe` Just ("", 'a')

        it "does not read char if condition not met" $ do
            f "bc" `shouldBe` Nothing

        it "returns nothing on underlying parser not matching" $ do
            f "" `shouldBe` Nothing
