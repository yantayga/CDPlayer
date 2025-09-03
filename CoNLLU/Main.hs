module Main where

import CoNLLU.CoNLLU

main :: IO ()
main = do
    h <- initCoNLLUDB
    putStrLn "Started."
    res <- loadDirectory h "../conllu"
    if res
    then do
        putStrLn "Success..."
        wc <- wordsCount h
        putStrLn ("Words: " ++ show wc)
        tc <- tagsCount h
        putStrLn ("Tags: " ++ show tc)
    else
        putStrLn "Failed"
    clearCoNLLUDB h
    putStrLn "Finished."
    return ()
