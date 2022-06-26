module X.Control.Parser.Text.Eof where

import X.Control.Parser
import X.Data.ParseError

-- | Matches the end-of-file only
eof :: Parser ()
eof =
    let parseEof "" = Right ("", ())
        parseEof s  = Left (ParseError "Expected end-of-input" s)
    in Parser parseEof
