module Utils where

import Control.Applicative

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

sconcat :: Semigroup s => [s] -> s
sconcat = foldr1 (<>)

mapMaybe :: Monad m => m b -> (a -> Maybe b) -> m a -> m b
mapMaybe d f a = a >>= \v -> maybe d return (f v)

mapMaybeOrEmpty :: (Monad m, Alternative m) => (a -> Maybe b) -> m a -> m b
mapMaybeOrEmpty = mapMaybe empty

(<$?>) :: (Monad m, Alternative m) => (a -> Maybe b) -> m a -> m b
(<$?>) = mapMaybeOrEmpty
