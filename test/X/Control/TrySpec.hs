module X.Control.TrySpec where
import Test.Hspec
import Harness.TestCase
import X.Control.Try
import Harness.Typeclass.FunctorCase
import X.Utils.Function
import Harness.Typeclass.MonadCase

spec :: Spec
spec = do
    monadDesc Success "Success"
    monadDesc (Failure "foo" >$ const) "Failure"
