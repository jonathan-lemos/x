{-# OPTIONS_GHC -F -pgmF htfpp #-}
module X.Control.TrySpec where

import Test.Framework hiding (Success, Failure)
import X.Control.Try
import X.Utils.LeftToRight
import Test.Framework.TestInterface
import TestUtils.Assertions.Typeclass.Monad
import TestUtils.Assertions.BasicAssertion

test_tryLaws :: Assertion
test_tryLaws = do
    verifyMonadLaws Success "Success"
    verifyMonadHappyPathUsage Success "Success"
    verifyMonadLaws (Failure "foo" @> const) "Failure"

test_trySemigroup :: Assertion
test_trySemigroup = basicAssertion $ do
    (Success "abc" <> Success "def") `shouldBe` Success "abcdef"
    (Success "abc" <> Failure "def") `shouldBe` Failure "def"
    (Failure "abc" <> Success "def") `shouldBe` Failure "abc"
    ((Failure "abc" :: Try String) <> Failure "def") `shouldBe` Failure "abc\ndef"
