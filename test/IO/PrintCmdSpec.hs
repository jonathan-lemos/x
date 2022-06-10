module IO.PrintCmdSpec where

import Test.Hspec
import IO.PrintCmd

spec :: Spec
spec = parallel $ do
    describe "PrintCmd tests" $ do
        it "appends newline for line" $ do
            line "foo" `shouldBe` PrintCmd [] "foo\n"

        it "prints newline for newline" $ do
            newline `shouldBe` PrintCmd [] "\n"