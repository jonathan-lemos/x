{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module X.TestUtils.MockTerminal (runPure) where
import X.Control.Terminal
import Control.Monad.Free

data MockTerminal a = MockTerminal
    { mtVal :: a
    , mtInput :: [String]
    , mtDims :: [TerminalDimensions]
    , mtOutput :: [String]
    }

instance Functor MockTerminal where
    fmap f (MockTerminal v i d o) = MockTerminal (f v) i d o

runPure :: [String] -> [TerminalDimensions] -> Terminal a -> [PrintCmd]
runPure _lines _dims (Pure _) = []
runPure (line:lines) dims (Free (InputLine f)) = runPure lines dims $ f (Just line)
runPure [] dims (Free (InputLine f)) = runPure [] dims $ f Nothing
runPure lines (dim:dims) (Free (Dimensions f)) = runPure lines dims $ f (Just dim)
runPure lines [] (Free (Dimensions f)) = runPure lines [] $ f Nothing
runPure lines dims (Free (Print cmd v)) = cmd : runPure lines dims v
runPure _ _ (Free Exit) = []
