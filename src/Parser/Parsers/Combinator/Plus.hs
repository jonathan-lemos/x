module Parser.Parsers.Combinator.Plus where
import Parser.Parser
import Parser.Parsers.Combinator.Atomic
import Control.Applicative

plus :: Parser a -> Parser [a]
plus = atomic . some
