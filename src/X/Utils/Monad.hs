module X.Utils.Monad where

import Control.Applicative

mapMaybe :: Monad m => m b -> (a -> Maybe b) -> m a -> m b
mapMaybe d f a = a >>= \v -> maybe d return (f v)

mapMaybeOrEmpty :: (Monad m, Alternative m) => (a -> Maybe b) -> m a -> m b
mapMaybeOrEmpty = mapMaybe empty

(<$?>) :: (Monad m, Alternative m) => (a -> Maybe b) -> m a -> m b
(<$?>) = mapMaybeOrEmpty
