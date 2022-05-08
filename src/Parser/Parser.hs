{-# LANGUAGE TupleSections #-}

module Parser.Parser where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Parser.Error

defaultErrMsg :: String
defaultErrMsg = "Syntax Error"

newtype Parser a = Parser
    { parse :: String -> Either ParseError (String, a)
    }

instance Functor Parser where
    fmap f p = Parser $ fmap (second f) . parse p

instance Applicative Parser where
    pure x = Parser $ Right . (,x)

    ab <*> a =
        Parser $ parse ab >=> \(r, f) -> second f <$> parse a r

instance Monad Parser where
    a >>= f =
        Parser $ parse a >=> \(newInput, aValue) -> parse (f aValue) newInput

instance MonadFail Parser where
    fail msg = Parser $ Left . ParseError msg

instance Semigroup t => Semigroup (Parser t) where
    a <> b =
        Parser $ parse a >=> \(newInput, aValue) -> second (aValue <>) <$> parse b newInput

instance Monoid t => Monoid (Parser t) where
    mempty = pure mempty

instance Alternative Parser where
    empty = Parser $ Left . ParseError defaultErrMsg
    f <|> g =
        Parser $ \s -> parse f s <> parse g s
