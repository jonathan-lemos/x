module X.Utils.Try where

import X.Control.Try

maybeToTry :: String -> Maybe a -> Try a
maybeToTry _ (Just v) = Success v
maybeToTry e Nothing = fail e
