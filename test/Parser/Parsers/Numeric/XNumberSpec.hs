module Parser.Parsers.Numeric.XNumberSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Numeric.Number
import Parser.Parsers.Numeric.XNumber (xnumber)
import Types.XNumber
import Types.XNumber (XNumber(XReal))

spec :: Spec 
spec = do
    describe "XNumber parser" $ do
        let f = parse xnumber

        it "parses integer on integer input" $ do
            f "123" `shouldBe` Just ("", XInteger 123)
            f "1abc" `shouldBe` Just ("abc", XInteger 1)

        it "parses double on double input" $ do
            f "4.5" `shouldBe` Just ("", XReal 4.5)
            f "0.5abc" `shouldBe` Just ("abc", XReal 0.5)

        it "parses nothing on non numeric" $ do
            f "" `shouldBe` Nothing
            f "abc" `shouldBe` Nothing
            