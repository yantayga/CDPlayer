{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module POS.HMM where

import Data.List (maximumBy)
import Data.Function (on)

import POS.HMMData

data Emissions e = Emissions [e]

type HiddenState = Int

type Emission = String

start :: h
start = undefined

hmm :: HMMData HiddenState Emission -> Emissions Emission -> ([HiddenState], Probability)
hmm ds es = maximumBy (compare `on` snd) $ evalHMM ds es

evalHMM :: HMMData HiddenState Emission -> Emissions Emission -> [([HiddenState], Probability)]
evalHMM ds (Emissions es) = foldl (stepHMM ds) [([start], 1.0)] es

stepHMM :: HMMData HiddenState Emission -> [([HiddenState], Probability)] -> Emission -> [([HiddenState], Probability)]
stepHMM ds ps e = concatMap (stepHMMForOneEmission ds e) ps

stepHMMForOneEmission :: HMMData HiddenState Emission -> Emission -> ([HiddenState], Probability)-> [([HiddenState], Probability)]
stepHMMForOneEmission ds e ((a:aps), p) = map addProbabilities hs
    where
        HiddenStates hs = hiddenStates ds
        addProbabilities :: HiddenState -> ([HiddenState], Probability)
        addProbabilities h = (h:aps, p * phh a h * phe h e)
        phh h1 h2 = transition (transitions ds) h1 h2
        phe h2 e = emission (emissions ds) h2 e
stepHMMForOneEmission ds e ps = error "Unknown"
