{-# LANGUAGE TupleSections #-}

module Parser.Parser where

import Control.Applicative
import Control.Monad
import Data.Bifunctor

data Parser t = Parser
    { name :: String
    , expected :: [String]
    , parse :: String -> Maybe (String, t)
    }

instance Functor Parser where
    fmap f p = p{parse = fmap (second f) . parse p}

instance Applicative Parser where
    pure x = Parser "" [] $ Just . (,x)

    ab <*> a =
        ab{parse = parse ab >=> \(r, f) -> second f <$> parse a r}

instance Monad Parser where
    a >>= f =
        a{parse = parse a >=> \(newInput, aValue) -> parse (f aValue) newInput}

instance Semigroup t => Semigroup (Parser t) where
    a <> b =
        Parser
            { name = concat [name a, " + ", name b]
            , expected = expected a <> expected b
            , parse = parse a >=> \(newInput, aValue) -> second (aValue <>) <$> parse b newInput
            }

instance Monoid t => Monoid (Parser t) where
    mempty = Parser "" [] $ Just . (,mempty)

instance Alternative Parser where
    empty = Parser "" [] $ const Nothing
    f <|> g =
        Parser
            { name = concat [name f, " | ", name g]
            , expected = expected f ++ expected g
            , parse = \s -> parse f s <|> parse g s
            }

instance Show (Parser a) where
    show = name

rename :: String -> Parser a -> Parser a
rename s p = p{name = s}

setExpected :: [String] -> Parser a -> Parser a
setExpected e p = p{expected = e}
