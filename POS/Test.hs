{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module POS.Test where

import qualified Data.Vector as V
import Data.Maybe (fromJust)

import POS.HMM.Types
import POS.HMM.HMM
import POS.HMM.Viterbi
import POS.HMM.Training

import POS.CoNLLU.Types
import POS.CoNLLU.Parse
import POS.CoNLLU.Training

loadCoNLLU :: FilePath -> IO (Maybe CoNLLUData)
loadCoNLLU fn = do
    content <- readFile fn
    return $ parseCoNLLU content

