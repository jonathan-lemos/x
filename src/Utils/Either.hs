module Utils.Either where

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe left Nothing = Left left
eitherFromMaybe _ (Just right) = Right right
