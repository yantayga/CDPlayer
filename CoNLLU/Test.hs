{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector.Strict as V
import Data.Maybe
import Data.Tree
import Data.Tuple
import Control.Monad
import Control.DeepSeq

import POS.HMM.Types
import POS.HMM.HMM
import POS.HMM.Viterbi
import POS.HMM.Training

import CoNLLU.Types
import CoNLLU.Parse
import CoNLLU.Load

drawDepTree m ss = drawTree $ drawDepTree' m (V.toList $ items ss) Nothing

drawDepTree' m ws w = Node {rootLabel = T.unpack $ fromJust $ M.lookup wid m, subForest = map (drawDepTree' m ws . Just) ws'}
    where
        !root = case w of
            Nothing -> fst $ wordId $ head $ filter ((== 0). depHead) ws
            Just w' -> fst $ wordId w'
        !wid = word (ws !! root)
        !ws' = filter ((== root). depHead) ws

printSentences db ss = do
    mapM_ (\(k, v) -> putStr (show k ++ ":") >> putStrLn (T.unpack v)) $ M.toList m
    res <- stats
    print res
    where
        m = invertBijection $ fullWords db
        stats = mapM_ (print . V.foldl' sumAll 0 . items) ss
        sumAll accN w = force $ accN + word w + initialWord w + uposTag w + xposTag w + depHead w + depRel w

invertBijection :: (Ord k, Ord v) => M.Map k v -> M.Map v k
invertBijection = M.foldrWithKey (flip M.insert) M.empty

main = do
    (!logs, !db) <- loadDirectory emptyDB "../conllu/"
    print "Loaded..."
    print logs
    printSentences db $ sentences db
--    print db