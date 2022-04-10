module Parser.Parsers.LiteralSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Literal

spec :: Spec
spec = do
    describe "Literal parser" $ do
        it "reads literal if matches" $ do
            let f = parse $ literal "abc"
            f "abcd" `shouldBe` Just ("d", "abc")
            f "abc" `shouldBe` Just ("", "abc")

        it "returns nothing on no match" $ do
            let f = parse $ literal "abc"
            f "abdc" `shouldBe` Nothing
            f "ab" `shouldBe` Nothing
            f "abd" `shouldBe` Nothing

        it "doesn't advance on blank string" $ do
            let f = parse $ literal ""
            f "" `shouldBe` Just ("", "")
            f "ab" `shouldBe` Just ("ab", "")
