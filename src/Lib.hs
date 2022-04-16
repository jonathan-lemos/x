module Lib where
import Shell

repl :: IO ()
repl = do
    prompt
    line <- getLine 
    case execute line of
        Left errMsg -> putStrLn errMsg
        Right val -> putStrLn val
    repl
