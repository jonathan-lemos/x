module X.TestUtils.Try where
import X.Control.Try

isSuccess :: Try a -> Bool
isSuccess (Success _) = True
isSuccess _ = False
