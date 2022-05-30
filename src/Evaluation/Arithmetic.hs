module Evaluation.Arithmetic where

import Control.Applicative
import Data.Number.CReal
import Evaluation.ToValue
import State.Value
import State.XState
import Utils.Either
import Unit.Unit
import Unit.Context
import Unit.UnitLike

_getValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String (Value, Value)
_getValues a b = liftA2 combineErrors (toValue a) (toValue b)

_evalValues :: (ToValue a, ToValue b) => (CReal -> Unit -> CReal -> Unit -> Either String Value) -> a -> b -> XState -> Either String Value
_evalValues f a b state =
    combineErrors (toValue a state) (toValue b state) >>=
        \(Numeric xa ua, Numeric xb ub) -> f xa ua xb ub

_evalAdditive :: (ToValue a, ToValue b) => (CReal -> CReal -> CReal) -> a -> b -> XState -> Either String Value
_evalAdditive f a b state =
    let addF xa ua xb ub =
            castUnit ua ub >>=
                \scaleB -> Right $ Numeric (f xa (scaleB xb)) ub
        in _evalValues addF a b state

addValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
addValues = _evalAdditive (+)

subValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
subValues = _evalAdditive (-)

multValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
multValues =
    let mulF xa ua xb ub =
            let newUnit = ua `unitMult` ub
                scale u x = x / toScale u * toScale newUnit
            in Right $ Numeric (scale ua xa * scale ub xb) newUnit
    in _evalValues mulF

divValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
divValues =
    let divF xa ua xb ub =
            let newUnit = ua `unitDiv` ub
                scale u x = x / toScale u * toScale newUnit
            in case (scale ua xa, scale ub xb) of
                (_, 0) -> Left "Cannot divide by 0"
                (a, b) -> Right $ Numeric (a / b) newUnit
    in _evalValues divF

expValues :: (ToValue a, ToValue b) => a -> b -> XState -> Either String Value
expValues =
    let expF xa ua xb ub =
            if ub /= unitless
                then Left "Cannot exponentiate by a unit quantity"
                else Right $ Numeric (xa ** xb) (ua `unitExpScalar` xb)
    in _evalValues expF
