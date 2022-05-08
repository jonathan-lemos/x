{-# LANGUAGE TupleSections #-}

module Parser.Parser where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Parser.Error
import State.State

defaultErrMsg :: String
defaultErrMsg = "Syntax Error"

newtype Parser a = Parser
    { parse :: XState -> String -> Either ParseError (String, a)
    }

instance Functor Parser where
    fmap f p = Parser $ fmap (second $ second f) . parse p

instance Applicative Parser where
    pure x = Parser $ \state input -> Right (input, x)

    ab <*> a =
        Parser $ \state input ->
            parse ab state input
                >>= \(newInput, func) -> second func <$> parse a state newInput

instance Monad Parser where
    a >>= f =
        Parser $ \state input ->
            parse a state input
                >>= \(newInput, aValue) -> parse (f aValue) state newInput

instance MonadFail Parser where
    fail msg = Parser $ const (Left . ParseError msg)

instance Semigroup t => Semigroup (Parser t) where
    a <> b =
        Parser $ \state input ->
            parse a state input
                >>= \(newInput, aValue) -> second (aValue <>) <$> parse b state newInput

instance Monoid t => Monoid (Parser t) where
    mempty = pure mempty

instance Alternative Parser where
    empty = Parser $ const (Left . ParseError defaultErrMsg)
    f <|> g =
        Parser $ \s -> parse f s <> parse g s
