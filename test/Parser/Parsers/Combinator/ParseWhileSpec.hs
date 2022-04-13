module Parser.Parsers.Combinator.ParseWhileSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Combinator.ParseWhile
import Parser.Parsers.Text.CharEq
    
spec :: Spec
spec = do
    let f = parse $ parseWhile (charEq 'a')

    describe "parse while" $ do
        it "parses while the parser succeeds" $ do
            f "aaabc" `shouldBe` Just ("bc", "aaa")
            f "abc" `shouldBe` Just ("bc", "a")
            f "aaa" `shouldBe` Just ("", "aaa")

        it "succeeds with empty list if parser doesn't match" $ do
            f "bc" `shouldBe` Just ("bc", "")
