module X.Utils.LTR where

-- | Takes a value and applies it to a function. Flipped version of $.
(@>) :: a -> (a -> b) -> b
(@>) = flip ($)
infixl 1 @>

-- | Takes the given Functor and applies the given function within. Flipped version of <$>.
(|@>|) :: Functor f => f a -> (a -> b) -> f b
(|@>|) = flip fmap
infixl 1 |@>|

-- | Takes the given Functor of Functor and applies the function two layers within. Flipped version of (fmap . fmap).
(||@>||) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(||@>||) = flip (fmap . fmap)
infixl 1 ||@>||
