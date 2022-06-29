module X.Utils.Functor where

-- Takes the given functor and applies the given function within. The flipped version of <$>
(>$>) :: Functor f => f a -> (a -> b) -> f b
(>$>) = flip fmap
infixl 4 >$>
