module Main where

import CoNLLU.CoNLLU

main :: IO ()
main = do
    h <- initCoNLLUDB
    putStrLn "Started."
    res <- loadDirectory h "../conllu"
    if res then putStrLn "Success..." else putStrLn "failed"
    clearCoNLLUDB h
    putStrLn "Finished."
    return ()
