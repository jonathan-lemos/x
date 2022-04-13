module Parser.Parsers.Combinator.ParseWhileAggregateSpec where


import Test.Hspec
import Parser.Parser
import Parser.Parsers.Combinator.ParseWhileAggregate
import Parser.Parsers.Combinator.Conditional
import Parser.Parsers.Text.CharEq
    
spec :: Spec
spec = do
    let f = parse $ parseWhileAggregate (charEq 'a') ((<= 2) . length)

    describe "parse while" $ do
        it "parses while the condition is true and parser succeeds" $ do
            f "aaabc" `shouldBe` Just ("abc", "aa")
            f "abaabc" `shouldBe` Just ("baabc", "a")

        it "reaches end if condition never reached" $ do
            f "a" `shouldBe` Just ("", "a")
            f "aa" `shouldBe` Just ("", "aa")

        it "succeeds with empty list if parser doesn't match" $ do
            f "bc" `shouldBe` Just ("bc", "")
            f "" `shouldBe` Just ("", "")
