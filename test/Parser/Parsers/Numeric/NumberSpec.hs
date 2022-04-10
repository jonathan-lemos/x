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

        let g = parse double

        it "reads a double at front" $ do
            g "0.25abc" `shouldBe` Just ("abc", 0.25)
            g "2.0a" `shouldBe` Just ("a", 2.0)
            g "0.0" `shouldBe` Just ("", 0.0)
            g ".2" `shouldBe` Just ("", 0.2)

        it "doesn't read invalid strings" $ do
            g "abc" `shouldBe` Nothing
            g "" `shouldBe` Nothing
            g "0" `shouldBe` Nothing
            g "1." `shouldBe` Nothing
