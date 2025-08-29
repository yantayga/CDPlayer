{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module POS.Test where

import qualified Data.Vector as V
import Data.Maybe (fromJust)

import POS.HMM.Types
import POS.HMM.HMM
import POS.HMM.Viterbi
import POS.HMM.Training
import POS.Conluu

loadConluu :: FilePath -> IO (Maybe ConluuData)
loadConluu fn = do
    content <- readFile fn
    return $ parseConluu content

