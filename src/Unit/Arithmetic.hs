module Unit.Arithmetic where

import Unit.Unit
import Data.Number.CReal
import Unit.UnitScaleOperation
import Control.Applicative
import Data.List
import Data.Foldable

unitMult :: Unit -> Unit -> Unit
unitMult a b = ProductUnit (_showInfixOp a "*" b) [a, b]

unitMaybeMult :: Maybe Unit -> Maybe Unit -> Maybe Unit
unitMaybeMult (Just a) (Just b) = Just $ unitMult a b
unitMaybeMult a b = a <|> b

unitDiv :: Unit -> Unit -> Unit
unitDiv a b = ProductUnit (_showInfixOp a "/" b) [a, unitExpScalar b (-1)]

unitMaybeDiv :: Maybe Unit -> Maybe Unit -> Maybe Unit
unitMaybeDiv (Just a) (Just b) = Just $ unitDiv a b
unitMaybeDiv (Just a) Nothing = Just a
unitMaybeDiv Nothing (Just b) = Just $ unitReciprocal b
unitMaybeDiv Nothing Nothing = Nothing

unitReciprocal :: Unit -> Unit
unitReciprocal u = ScaledUnit (_showInfixOp (1 :: Integer) "/" u) (Exponentiate (-1)) u

unitMaybeReciprocal :: Maybe Unit -> Maybe Unit
unitMaybeReciprocal = fmap unitReciprocal

maybeScalar :: (Unit -> CReal -> Unit) -> Maybe Unit -> CReal -> Maybe Unit
maybeScalar f u s = (`f` s) <$> u

unitAddScalar :: Unit -> CReal -> Unit
unitAddScalar u s = ScaledUnit (_showInfixOp u "+" s) (Add s) u

unitMaybeAddScalar :: Maybe Unit -> CReal -> Maybe Unit
unitMaybeAddScalar = maybeScalar unitAddScalar

unitSubScalar :: Unit -> CReal -> Unit
unitSubScalar u s = ScaledUnit (_showInfixOp u "-" s) (Add (-s)) u

unitMaybeSubScalar :: Maybe Unit -> CReal -> Maybe Unit
unitMaybeSubScalar = maybeScalar unitSubScalar

unitMultScalar :: Unit -> CReal -> Unit
unitMultScalar u s = ScaledUnit (_showInfixOp u "*" s) (Multiply s) u

unitMaybeMultScalar :: Maybe Unit -> CReal -> Maybe Unit
unitMaybeMultScalar = maybeScalar unitMultScalar

unitDivScalar :: Unit -> CReal -> Unit
unitDivScalar u s = ScaledUnit (_showInfixOp u "/" s) (Multiply (1 / s)) u

unitMaybeDivScalar :: Maybe Unit -> CReal -> Maybe Unit
unitMaybeDivScalar = maybeScalar unitDivScalar

unitExpScalar :: Unit -> CReal -> Unit
unitExpScalar b e = ScaledUnit (_showInfixOp b "^" e) (Exponentiate e) b

unitMaybeExpScalar :: Maybe Unit -> CReal -> Maybe Unit
unitMaybeExpScalar = maybeScalar unitExpScalar

unitProduct :: (Foldable f) => f Unit -> Unit
unitProduct = liftA2 ProductUnit (intercalate "*" . fmap show) id. toList