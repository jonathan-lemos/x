module Utils.Either where

import Data.Either
import Data.Foldable
import Data.List

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe left Nothing = Left left
eitherFromMaybe _ (Just right) = Right right

concatErrors :: (Functor f, Foldable f) => f (Either String a) -> Either String [a]
concatErrors foldable =
    case partitionEithers (toList foldable) of
        ([], rs) -> Right rs
        (es, _) -> Left $ intercalate "\n" es

combineErrors :: Either String a -> Either String b -> Either String (a, b)
combineErrors a b =
    case (a, b) of
        (Left e1, Left e2) -> Left $ e1 <> "\n" <> e2
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right v1, Right v2) -> Right (v1, v2)
