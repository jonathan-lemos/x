module Evaluation.Arithmetic where

import Control.Applicative
import Data.Number.CReal
import Evaluation.ToValue
import State.Value
import State.XState
import Utils.Either
import Unit.Context
import Unit.ValueUnit

_getValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String (Value, Value)
_getValues a b = liftA2 combineErrors (toValue a) (toValue b)

_evalValues :: (ToValue a, ToValue b) => (CReal -> Maybe ValueUnit -> CReal -> Maybe ValueUnit -> Either String Value) -> a -> b -> XState -> Either String Value
_evalValues f a b state =
    combineErrors (toValue a state) (toValue b state) >>=
        \(Numeric xa ua, Numeric xb ub) -> f xa ua xb ub

_evalAdditive :: (ToValue a, ToValue b) => (CReal -> CReal -> CReal) -> a -> b -> XState -> Either String Value
_evalAdditive f a b state =
    let addF xa ua xb ub =
            castMaybeUnit ua ub >>=
                \scaleB -> Right $ Numeric (f xa (scaleB xb)) ub
        in _evalValues addF a b state

addValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
addValues = _evalAdditive (+)

subValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
subValues = _evalAdditive (-)

multValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
multValues =
    let mulF 

