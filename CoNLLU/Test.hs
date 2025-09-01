{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module CoNLLU.Test where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe
import Data.Tree
import Data.Tuple
import Control.Monad

import POS.HMM.Types
import POS.HMM.HMM
import POS.HMM.Viterbi
import POS.HMM.Training

import CoNLLU.Types
import CoNLLU.Parse
import CoNLLU.Training

loadCoNLLU :: FilePath -> IO (Maybe CoNLLUData)
loadCoNLLU fn = do
    content <- readFile fn
    return $ parseCoNLLU content

drawDepTree m ss = drawTree $ drawDepTree' m (items ss) Nothing

drawDepTree' m ws w = Node {rootLabel = fromJust $ M.lookup wid m, subForest = map (drawDepTree' m ws . Just) ws'}
    where
        root = case w of
            Nothing -> fst $ wordId $ head $ filter ((== 0). depHead) ws
            Just w' -> fst $ wordId w'
        wid = word (ws !! root)
        ws' = filter ((== root). depHead) ws

printSentences db ss = mapM_ (putStrLn . drawDepTree m) $ take 10 ss
    where
        m = M.fromList $ map swap $ M.toList $ fullWords db

testCONLLU = do 
    db <- loadCoNLLU "..."
    printSentences (fromJust db) $ sentences $ fromJust db
