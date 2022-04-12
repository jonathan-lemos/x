module Parser.Parsers.Text.CharSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Text.Char

spec :: Spec
spec = do
    describe "Char parser" $ do
        let f = parse char

        it "can read a char" $ do
            f "abc" `shouldBe` Just ("bc", 'a')
            f "a" `shouldBe` Just ("", 'a')

        it "returns nothing on no char" $ do
            f "" `shouldBe` Nothing
