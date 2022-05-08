module Parser.Parsers.Combinator.Atomic where

import Data.Bifunctor
import Parser.Error
import Parser.Parser
import Parser.Parsers.Combinator.MapResult

-- | If the parser fails, the error's `currentInput` will be set to its original input. In other words, it either parses successfully or parses no characters.
atomic :: Parser a -> Parser a
atomic = mapResultWithInput $ first . setCurrentInput
