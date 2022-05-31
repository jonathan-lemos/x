module Utils.Either where
import Data.Validation
import Data.Foldable
import Data.List
import Data.Bifunctor

eitherFromMaybe :: a -> Maybe b -> Either a b
eitherFromMaybe left Nothing = Left left
eitherFromMaybe _ (Just right) = Right right

eitherToValidation :: Either a b -> Validation a b
eitherToValidation (Left l) = Failure l
eitherToValidation (Right r) = Success r

concatErrors :: (Functor f, Foldable f) => f (Either String a) -> Either String [a]
concatErrors foldable =
    let validations = fmap (bimap (:[]) (:[]) . eitherToValidation) foldable
        in toEither . first (intercalate "\n") $ mconcat (toList validations)

combineErrors :: Either String a -> Either String b -> Either String (a, b)
combineErrors a b =
    case (a, b) of
        (Left e1, Left e2) -> Left $ e1 <> "\n" <> e2
        (Left e1, _) -> Left e1
        (_, Left e2) -> Left e2
        (Right v1, Right v2) -> Right (v1, v2)
