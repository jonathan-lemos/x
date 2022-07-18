module X.Utils.Functor where

-- Takes the given functor and applies the given function within. The flipped version of <$>
(>$>) :: Functor f => f a -> (a -> b) -> f b
(>$>) = flip fmap
infixl 4 >$>

-- Applies a function within two layers of Functor. The flipped version of (fmap . fmap)
(>>$>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(>>$>) = flip (fmap . fmap)
infixl 4 >>$>
