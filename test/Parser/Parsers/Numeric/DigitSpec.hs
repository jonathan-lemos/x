module Parser.Parsers.Numeric.DigitSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Numeric.Digit

spec :: Spec
spec = do
    describe "Digit parser" $ do
        let f = parse digit

        it "reads a digit at front" $ do
            f "123abc" `shouldBe` Just ("23abc", 1)
            f "9a" `shouldBe` Just ("a", 9)
            f "5" `shouldBe` Just ("", 5)

        it "does not read non digit" $ do
            f "bc" `shouldBe` Nothing
            f "" `shouldBe` Nothing
