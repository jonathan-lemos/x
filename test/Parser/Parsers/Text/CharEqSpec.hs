module Parser.Parsers.Text.CharEqSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Text.CharEq

spec :: Spec
spec = do
    describe "CharEq parser" $ do
        let f = parse $ charEq 'a'

        it "can read a char" $ do
            f "abc" `shouldBe` Just ("bc", 'a')
            f "a" `shouldBe` Just ("", 'a')

        it "returns nothing on wrong char" $ do
            f "bc" `shouldBe` Nothing

        it "returns nothing on no char" $ do
            f "" `shouldBe` Nothing
