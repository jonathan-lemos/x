module Parser.ParserSpec where

import Test.Hspec
import Parser.Parser
import Parser.Parsers.Text.Char
import Parser.Parsers.Numeric.Digit
import Control.Monad
import Control.Applicative
import Parser.Parsers.Numeric.Number
import Parser.Parsers.Text.CharEq (charEq)
import Parser.Parsers.Text.Literal (literal)

spec :: Spec
spec = do
    describe "parser functor" $ do
        it "functors correctly" $ do
            let f = parse $ (+1) <$> digit
            f "2" `shouldBe` Just ("", 3)

    describe "parser applicative" $ do
        it "applies function correctly" $ do
            let f = digit
            let g = pure (+1)
            let h = parse $ g <*> f

            h "2ghi" `shouldBe` Just ("ghi", 3)

        it "advances parser correctly" $ do
            let f = digit
            let g = replicateM_ 3 char >> pure (+1)
            let h = parse $ g <*> f

            h "abc2ghi" `shouldBe` Just ("ghi", 3)

        it "doesn't apply if one or the other is missing" $ do
            let f = parse $ (empty :: Parser (Char -> Int)) <*> char
            let g = parse $ pure id <*> (empty :: Parser Char)
            let h = parse $ (empty :: Parser (Char -> Int)) <*> (empty :: Parser Char)
            let s = "a"

            f s `shouldBe` Nothing
            g s `shouldBe` Nothing
            h s `shouldBe` Nothing

    describe "parser monad" $ do
        it "applies function correctly" $ do
            let f = digit
            let g c = pure $ c + 1
            let h = parse $ f >>= g

            h "2ghi" `shouldBe` Just ("ghi", 3)

        it "advances parser correctly" $ do
            let f = digit
            let g c = do
                        replicateM_ 3 char
                        return $ c + 1
            let h = parse $ f >>= g

            h "2abcghi" `shouldBe` Just ("ghi", 3)

        it "doesn't execute function if parser fails" $ do
            let f = parse $ empty >>= (undefined :: Char -> Parser Int)
            f "abc" `shouldBe` Nothing

        it "returns nothing if monad parser returns nothing" $ do
            let f = parse $ pure () >>= const (empty :: Parser Int)
            f "abc" `shouldBe` Nothing

    describe "parser alternative" $ do
        it "doesn't execute right if left succeeds" $ do
            let f = parse $ integer <|> undefined
            f "123abc" `shouldBe` Just ("abc", 123)

        it "chooses right if left fails" $ do
            let f = parse $ empty <|> integer
            f "123abc" `shouldBe` Just ("abc", 123)

        it "returns nothing if both fail" $ do
            let f = parse $ empty <|> (empty :: Parser Int)
            f "abc" `shouldBe` Nothing

    describe "parser monoid" $ do
        let f = parse $ literal "hello" <> literal "world"

        it "concatenates with <>" $ do
            f "helloworldbar" `shouldBe` Just ("bar", "helloworld")

        it "<> fails if left doesn't match" $ do
            f "helworldbar" `shouldBe` Nothing

        it "<> fails if right doesn't match" $ do
            f "helloworbar" `shouldBe` Nothing

        it "<> fails if neither matches" $ do
            f "bar" `shouldBe` Nothing

        it "mempty does nothing" $ do
            let g = parse (mempty :: Parser String)
            g "helloworldbar" `shouldBe` Just ("helloworldbar", "")
