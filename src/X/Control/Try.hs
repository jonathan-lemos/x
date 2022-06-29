{-# LANGUAGE MultiParamTypeClasses #-}
module X.Control.Try where

-- Represents a value that may be present, or one or more error messages.
data Try a = Success a | Failures [String]
    deriving (Show, Eq, Ord)

failmap :: ([String] -> [String]) -> Try a -> Try a
failmap _ (Success a) = Success a
failmap f (Failures fs) = Failures $ f fs

instance Functor Try where
    fmap f (Success a) = Success $ f a
    fmap _ (Failures fs) = Failures fs

instance Applicative Try where
    pure = Success
    (Success f) <*> (Success a) = Success $ f a
    (Success _) <*> (Failures fs) = Failures fs
    (Failures fs) <*> (Success _) = Failures fs
    (Failures fs1) <*> (Failures fs2) = Failures $ fs1 <> fs2

instance Monad Try where
    -- technically this is not a valid monad instance because ap != <*>
    -- ap will only keep the first failure, but <*> combines the failures from the left and right
    --
    -- but i want it to collect the errors w/ liftA2 but not with do-syntax, so this works out fine
    (Success a) >>= f = f a
    (Failures fs) >>= _ = Failures fs

instance MonadFail Try where
    fail = Failures . (:[])

instance Semigroup (Try a) where
    (Success a) <> (Success _) = Success a
    (Success a) <> (Failures _) = Success a
    (Failures _) <> (Success b) = Success b
    (Failures a) <> (Failures b) = Failures $ a <> b

instance Monoid a => Monoid (Try a) where
    mempty = Success mempty
