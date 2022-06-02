{-# LANGUAGE FlexibleContexts #-}
module Evaluation.Arithmetic where

import Control.Applicative
import Data.Number.CReal
import Evaluation.ToValue
import State.Value
import State.XState
import Unit.Unit
import Utils.Either
import Utils.String
import Unit.Arithmetic

_getValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String (Value, Value)
_getValues a b = liftA2 combineErrors (toValue a) (toValue b)

_evalValues :: (ToValue a, ToValue b) => (CReal -> Maybe Unit -> CReal -> Maybe Unit -> Either String Value) -> a -> b -> XState -> Either String Value
_evalValues f a b state =
    combineErrors (toValue a state) (toValue b state)
        >>= \(Numeric xa ua, Numeric xb ub) -> f xa ua xb ub

_evalAdditive :: (ToValue a, ToValue b) => (CReal -> CReal -> CReal) -> String -> a -> b -> XState -> Either String Value
_evalAdditive f verb a b state =
    let addF xa ua xb ub =
            (castMaybeUnit ua ub <> Left ("Cannot " <> verb <> " " <> show ua <> " and " <> show ub))
                >>= \scaleB -> Right $ Numeric (f xa (scaleB xb)) ub
     in _evalValues addF a b state

addValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
addValues = _evalAdditive (+) "add"

subValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
subValues = _evalAdditive (-) "subtract"

_unitMaybeifyable :: (Unit -> Unit -> Unit) -> (Unit -> Unit -> Either String (Maybe Unit))
_unitMaybeifyable f a b = Right . Just $ f a b

multValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
multValues =
    let mulF xa ua xb ub =
            let newUnit = ua `unitMaybeMult` ub
                scale a = scaleMaybeUnit a newUnit
            in Right $ Numeric (scale ua xa * scale ub xb) newUnit
     in _evalValues mulF

divValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
divValues =
    let divF xa ua xb ub =
            let newUnit = ua `unitMaybeDiv` ub
                scale a = scaleMaybeUnit a newUnit
             in case (scale ua xa, scale ub xb) of
                    (_, 0) -> Left "Cannot divide by 0"
                    (a, b) -> Right $ Numeric (a / b) newUnit
     in _evalValues divF

expValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
expValues =
    let expF xa ua xb ub =
            case ub of
                Nothing -> Right $ Numeric (xa ** xb) ((`unitExpScalar` xb) <$> ua)
                Just u -> Left $ "Cannot exponentiate by a unit quantity " <> parenthesize (show u) <> ". Must exponentiate by a unitless quantity."
     in _evalValues expF
