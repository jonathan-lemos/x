module TestUtils.Should where
import Test.Hspec
import Control.Monad

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
