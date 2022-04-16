module Parser.Parsers.Numeric.NumberSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Numeric.Number

spec :: Spec
spec = do
    describe "Numeric parsers" $ do
        let f = parse integer

        it "reads an integer at front" $ do
            f "123abc" `shouldBe` Just ("abc", 123)
            f "9a" `shouldBe` Just ("a", 9)
            f "567" `shouldBe` Just ("", 567)

        it "does not read non number" $ do
            f "bc" `shouldBe` Nothing
            f "" `shouldBe` Nothing
