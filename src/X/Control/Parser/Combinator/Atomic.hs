module X.Control.Parser.Combinator.Atomic where
import X.Control.Parser.Combinator.MapResult
import X.Control.Parser
import Data.Bifunctor
import X.Data.ParseError

-- | If the parser fails, sets the ParseError location to the original input. In other words, it either parses an entire value or nothing
atomic :: Parser a -> Parser a
atomic = mapResultWithInput $ first . setCurrentInput
