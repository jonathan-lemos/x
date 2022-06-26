module X.Control.Parser.Combinator.Choice.LookaheadParse where
import X.Control.Parser
import X.Control.Parser.Combinator.Choice.MetaParse
import Data.Foldable
import Control.Applicative

-- | Given a list of metaparsers, gets the output of the first metaparser that succeeds, or the last error if they all fail.
lookaheadParse :: [Parser (Parser a)] -> Parser a
lookaheadParse = metaParse . foldl' (<|>) empty
