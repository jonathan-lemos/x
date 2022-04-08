
module Parser.Parsers.ConditionalSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Char
import Parser.Parsers.Conditional

spec :: Spec
spec = do
    describe "Conditional parser" $ do
        let f = parse $ conditional char (== 'a')

        it "reads char if condition is met" $ do
            f "abc" `shouldBe` Just ("bc", 'a')
            f "a" `shouldBe` Just ("", 'a')

        it "does not read char if condition not met" $ do
            f "bc" `shouldBe` Nothing

        it "returns nothing on no char" $ do
            f "" `shouldBe` Nothing
