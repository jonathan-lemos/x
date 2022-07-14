{-# LANGUAGE MultiParamTypeClasses #-}
module X.Control.Try where
import Control.Applicative

combineErrorMessages :: String -> String -> String
combineErrorMessages "" s = s
combineErrorMessages s "" = s
combineErrorMessages a b = a <> "\n" <> b

-- Represents a value that may be present, or one or more error messages.
data Try a = Success a | Failure String
    deriving (Show, Eq, Ord)

failmap :: (String -> String) -> Try a -> Try a
failmap _ (Success a) = Success a
failmap f (Failure reason) = Failure $ f reason

instance Functor Try where
    fmap f (Success a) = Success $ f a
    fmap _ (Failure f) = Failure f

instance Applicative Try where
    pure = Success
    (Success f) <*> (Success a) = Success $ f a
    (Success _) <*> (Failure reason) = Failure reason
    (Failure reason) <*> (Success _) = Failure reason
    (Failure reason1) <*> (Failure reason2) = Failure $ combineErrorMessages reason1 reason2

instance Monad Try where
    -- technically this is not a valid monad instance because ap != <*>
    -- ap will only keep the first failure, but <*> combines the failures from the left and right
    --
    -- but i want it to collect the errors w/ liftA2 but not with do-syntax, so this works out fine
    (Success a) >>= f = f a
    (Failure reason) >>= _ = Failure reason

instance MonadFail Try where
    fail = Failure

instance Semigroup a => Semigroup (Try a) where
    Success a <> Success b = Success (a <> b)
    Failure a <> Failure b = Failure $ combineErrorMessages a b
    Failure a <> Success _ = Failure a
    Success _ <> Failure b = Failure b

instance Monoid a => Monoid (Try a) where
    mempty = Success mempty

instance Alternative Try where
    empty = Failure ""
    Success a <|> _ = Success a
    Failure _ <|> Success b = Success b
    Failure _ <|> Failure b = Failure b