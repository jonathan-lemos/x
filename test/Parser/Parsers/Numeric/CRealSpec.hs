module Parser.Parsers.Numeric.CRealSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Numeric.CReal

spec :: Spec 
spec = do
    describe "CReal parser" $ do
        let f = parse creal

        it "parses integer on integer input" $ do
            f "123" `shouldBe` Just ("", 123)
            f "1abc" `shouldBe` Just ("abc", 1)

        it "parses double on double input" $ do
            f "4.5" `shouldBe` Just ("", read "4.5")
            f "0.5abc" `shouldBe` Just ("abc", read "0.5")
            f "0.31415e2" `shouldBe` Just ("", read "31.415")
            f "55e-4" `shouldBe` Just ("", read "0.0055")
            f "-4.2" `shouldBe` Just ("", read "-4.2")

        it "parses nothing on non numeric" $ do
            f "" `shouldBe` Nothing
            f "abc" `shouldBe` Nothing
            