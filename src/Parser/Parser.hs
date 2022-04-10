{-# LANGUAGE TupleSections #-}
module Parser.Parser where

import Control.Monad
import Utils (mapSnd)
import Control.Applicative

newtype Parser t = Parser {parse :: String -> Maybe (String, t)}

instance Functor Parser where
    fmap f (Parser pf) = Parser $ fmap (mapSnd f) . pf

instance Applicative Parser where
    pure x = Parser $ Just . (, x)

    (Parser ab) <*> (Parser a) =
        Parser $ ab >=> \(r, f) -> mapSnd f <$> a r

instance Monad Parser where
    (Parser a) >>= f =
        Parser $ transform <=< a where
            transform (newInput, aValue) = parse (f aValue) newInput

instance Semigroup t => Semigroup (Parser t) where
    (Parser a) <> (Parser b) =
        Parser $ transform <=< a where
            transform (newInput, aValue) = mapSnd (aValue <>) <$> b newInput

instance Monoid t => Monoid (Parser t) where
    mempty = Parser $ Just . (, mempty)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser f) <|> (Parser g) =
        Parser $ \s -> f s <|> g s
