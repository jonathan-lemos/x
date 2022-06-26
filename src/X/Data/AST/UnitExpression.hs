module X.Data.AST.UnitExpression where

import Data.List
import Data.Number.CReal
import X.Data.Unit.Unit
import X.Data.State.XState
import X.Utils.Either
import Control.Applicative
import X.Data.Unit.Arithmetic

-- | A base unit term. Either `unit^power` or just `unit`.
data UnitFactor = UnitPower String CReal | JustUnit String
    deriving Eq

ufToUnit :: UnitFactor -> XState -> Either String Unit
ufToUnit (JustUnit s) state =
    eitherFromMaybe ("No such unit " <> show s) $ getUnit s state
ufToUnit (UnitPower b e) state =
    (`unitExpScalar` e) <$> ufToUnit (JustUnit b) state

instance Show UnitFactor where
    show (UnitPower unit real) = show unit <> "^" <> show real
    show (JustUnit unit) = show unit

-- | A chain of unit multiplications.
data UnitMultExpression = UnitMultExpression UnitFactor [UnitFactor]
    deriving Eq

instance Show UnitMultExpression where
    show (UnitMultExpression x xs) = intercalate "*" $ show <$> x:xs

umToUnit :: UnitMultExpression -> XState -> Either String Unit
umToUnit (UnitMultExpression x xs) state =
    unitProduct <$> concatErrors ((`ufToUnit` state) <$> (x:xs))

-- | A chain of unit multiplications or a fraction.
data UnitExpression = UnitFraction UnitMultExpression UnitMultExpression | UnitProduct UnitMultExpression
    deriving Eq

instance Show UnitExpression where
    show (UnitFraction ua ub) = show ua <> "/" <> show ub
    show (UnitProduct e) = show e

ueToUnit :: UnitExpression -> XState -> Either String Unit
ueToUnit (UnitProduct e) state = umToUnit e state
ueToUnit (UnitFraction ua ub) state = liftA2 unitDiv (umToUnit ua state) (umToUnit ub state)
