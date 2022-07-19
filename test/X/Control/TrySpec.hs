module X.Control.TrySpec where

import Test.Hspec
import Harness.TestCase
import X.Control.Try
import Harness.Typeclass.FunctorCase
import Harness.Typeclass.MonadCase
import X.Utils.LeftToRight

spec :: Spec
spec = do
    verifyMonadLaws Success "Success"
    verifyMonadHappyPathUsage Success "Success"
    verifyMonadLaws (Failure "foo" @> const) "Failure"

    desc "Try semigroup" $ do
        (Success "abc" <> Success "def") `shouldEq` Success "abcdef"
        (Success "abc" <> Failure "def") `shouldEq` Failure "def"
        (Failure "abc" <> Success "def") `shouldEq` Failure "abc"
        ((Failure "abc" :: Try String) <> Failure "def") `shouldEq` Failure "abc\ndef"

