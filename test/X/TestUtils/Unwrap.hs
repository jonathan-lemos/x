{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module X.TestUtils.Unwrap where
import Data.Maybe
import X.Control.Try

class Unwrap f where
    unwrap :: f a -> a

instance Unwrap Maybe where
    unwrap = fromJust

instance Unwrap (Either a) where
    unwrap (Right v) = v

instance Unwrap Try where
    unwrap (Success v) = v

instance Unwrap [] where
    unwrap (x:xs) = x
