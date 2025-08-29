{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module POS.Test where

import POS.HMM.Types
import POS.HMM.HMM
import POS.HMM.Viterbi
import POS.Conluu

loadConluu :: FilePath -> IO (Maybe ConluuData)
loadConluu fn = do
    content <- readFile fn
    return $ parseConluu content