module X.Control.Parser.Combinator.Complete where

import X.Control.Parser
import X.Control.Parser.Combinator.MapResult
import X.Data.ParseError

-- Returns a parser that must match the complete input.
complete :: Parser a -> Parser a
complete =
    let mapSuccessfulParse (remainingInput, value) =
            case remainingInput of
                "" -> Right ("", value)
                x -> Left (ParseError "Unexpected end of parse" x)
    in mapResult (>>= mapSuccessfulParse)
