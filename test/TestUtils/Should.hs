module TestUtils.Should where
import Test.Hspec
import Control.Monad
import Test.Hspec.QuickCheck
import Test.QuickCheck

shouldBeSpec :: (Show a, Eq a) => String -> [(a, a, String)] -> SpecWith ()
shouldBeSpec title as =
    describe title $ do
        forM_ as $ \(actual, expected, name) ->
            it name $ do
                actual `shouldBe` expected

shouldSatisfySpec :: (Show a, Eq a) => String -> [(a, a -> Bool, String)] -> SpecWith ()
shouldSatisfySpec title as =
    describe title $ do
        forM_ as $ \(actual, predicate, name) ->
            it name $ do
                actual `shouldSatisfy` predicate

shouldSatisfyQcSpec :: (Arbitrary a, Show a, Show b, Eq b) => String -> [(a -> b, a -> b -> Bool, String)] -> SpecWith ()
shouldSatisfyQcSpec title as =
    describe title $ do
        forM_ as $ \(func, predicate, name) ->
            prop name $ do
                \x -> func x `shouldSatisfy` predicate x

shouldBeQcSpec :: (Arbitrary a, Show a, Show b, Eq b) => String -> [(a -> b, a -> b, String)] -> SpecWith ()
shouldBeQcSpec title as =
    describe title $ do
        forM_ as $ \(actual, expected, name) ->
            prop name $ do
                \x -> actual x `shouldBe` expected x
