module IO.IOCmdSpec where

import Test.Hspec
import IO.IOCmd

spec :: Spec
spec = do
    describe "IOCmd tests" $ do
        it "appends newline for line" $ do
            line "foo" `shouldBe` IOCmd [] "foo\n"

        it "prints newline for newline" $ do
            newline `shouldBe` IOCmd [] "\n"