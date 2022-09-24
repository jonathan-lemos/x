{-# OPTIONS_GHC -F -pgmF htfpp #-}
module X.Control.ParserSpec where

import Test.Framework
import Control.Applicative
import Control.Monad
import X.Control.Parser
import X.Control.Parser.Numeric.Digit
import X.Control.Parser.Numeric.Number
import X.Control.Parser.Text.Char
import X.Control.Parser.Text.CharEq (charEq)
import X.Control.Parser.Text.Literal (literal)
import X.Data.ParseError
import Test.Framework.TestInterface
import TestUtils.Assertions.BasicAssertion

test_parserFunctor :: Assertion
test_parserFunctor = basicAssertion $ do
    let f = parse $ (+ 1) <$> digit
    f "2" `shouldBe` Right ("", 3)
    f "2 foo" `shouldBe` Right (" foo", 3)

test_parserApplicativeApplication :: Assertion
test_parserApplicativeApplication = basicAssertion $ do
    let f = digit
    let g = pure (+ 1)
    let h = parse $ g <*> f

    h "2ghi" `shouldBe` Right ("ghi", 3)

test_parserApplicativeAdvance :: Assertion
test_parserApplicativeAdvance = basicAssertion $ do
    let f = digit
    let g = replicateM_ 3 char >> pure (+ 1)
    let h = parse $ g <*> f

    h "abc2ghi" `shouldBe` Right ("ghi", 3)

test_parserApplicativeDoesNotApplyIfEitherFails :: Assertion
test_parserApplicativeDoesNotApplyIfEitherFails = basicAssertion $ do
    let f = parse $ (empty :: Parser (Char -> Int)) <*> char
    let g = parse $ pure id <*> (empty :: Parser Char)
    let h = parse $ (empty :: Parser (Char -> Int)) <*> (empty :: Parser Char)
    let s = "a"

    f s `shouldBe` Left (ParseError "Syntax Error" "a")
    g s `shouldBe` Left (ParseError "Syntax Error" "a")
    h s `shouldBe` Left (ParseError "Syntax Error" "a")

test_parserMonadApplication :: Assertion
test_parserMonadApplication = basicAssertion $ do
    let f = digit
    let g c = pure $ c + 1
    let h = parse $ f >>= g

    h "2ghi" `shouldBe` Right ("ghi", 3)

test_parserMonadAdvance :: Assertion
test_parserMonadAdvance = basicAssertion $ do
    let f = digit
    let g c = do
            replicateM_ 3 char
            return $ c + 1
    let h = parse $ f >>= g

    h "2abcghi" `shouldBe` Right ("ghi", 3)

test_parserMonadDoesNotExecuteFunctionOnParserFailure :: Assertion
test_parserMonadDoesNotExecuteFunctionOnParserFailure = basicAssertion $ do
    let f = parse $ empty >>= (undefined :: Char -> Parser Int)
    f "abc" `shouldBe` Left (ParseError "Syntax Error" "abc")

test_parserMonadFailsIfFunctionFails :: Assertion
test_parserMonadFailsIfFunctionFails = basicAssertion $ do
    let f = parse $ pure () >>= const (empty :: Parser Int)
    f "abc" `shouldBe` Left (ParseError "Syntax Error" "abc")

test_parserAlternativeShortCircuits :: Assertion
test_parserAlternativeShortCircuits = basicAssertion $ do
    let f = parse $ integer <|> undefined
    f "123abc" `shouldBe` Right ("abc", 123)

test_parserAlternativeChoosesRightIfLeftFails :: Assertion
test_parserAlternativeChoosesRightIfLeftFails = basicAssertion $ do
    let f = parse $ empty <|> integer
    f "123abc" `shouldBe` Right ("abc", 123)

test_parserAlternativeReturnsRightErrorIfBothFail :: Assertion
test_parserAlternativeReturnsRightErrorIfBothFail = basicAssertion $ do
    let f = parse $ empty <|> (fail "foo bar" :: Parser Int)
    f "abc" `shouldBe` Left (ParseError "foo bar" "abc")

test_parserMonoidConcatenation :: Assertion
test_parserMonoidConcatenation = basicAssertion $ do
    let f = parse $ literal "hello" <> literal "world"
    f "helloworldbar" `shouldBe` Right ("bar", "helloworld")

test_parserMonoidFailsIfEitherDoesntMatch :: Assertion
test_parserMonoidFailsIfEitherDoesntMatch = basicAssertion $ do
    let f = parse $ literal "hello" <> literal "world"
    f "helworldbar" `shouldBe` Left (ParseError "Expected \"hello\"" "worldbar")
    f "helloworbar" `shouldBe` Left (ParseError "Expected \"world\"" "bar")
    f "bar" `shouldBe` Left (ParseError "Expected \"hello\"" "bar")

test_parserMonoidMemptyDoesNothing :: Assertion
test_parserMonoidMemptyDoesNothing = basicAssertion $ do
    let g = parse (mempty :: Parser String)
    g "helloworldbar" `shouldBe` Right ("helloworldbar", "")
