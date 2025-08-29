{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module POS.HMM.Viterbi where

import Data.List (maximumBy)
import Data.Function (on)

import POS.HMM.Types

hmm :: (Eq e, Ord p, Num p) => HMMDataT h e p -> [e] -> HMMResultT h p
hmm hmmData es = unpackResult hmmData $ maximumBy (compare `on` snd) $ evalHMM hmmData ies
    where
        ies = emissionsToIndexes hmmData es

evalHMM :: (Ord p, Num p) => HMMDataT h e p -> [Int] -> [HMMIndexedResultT p]
evalHMM hmmData = foldl (stepHMM hmmData) [([startState hmmData], initialProbability hmmData)]

stepHMM :: (Ord p, Num p) => HMMDataT h e p -> [HMMIndexedResultT p] -> Int -> [HMMIndexedResultT p]
stepHMM hmmData ps ie = map (maximumBy (compare `on` snd) . stepHMMForOneEmission hmmData ie) ps

stepHMMForOneEmission :: Num p => HMMDataT h e p -> Int -> HMMIndexedResultT p-> [HMMIndexedResultT p]
stepHMMForOneEmission hmmData ie (ihs@(ih1:_), p) = map addProbabilities newIhs
   where
        newIhs = [0 .. hiddenStatesSize hmmData]
        addProbabilities ih2 = (ih2:ihs, p * ph2h ih1 ih2 * ph2e ih1 ie)
        ph2h = transition hmmData
        ph2e = emission hmmData
stepHMMForOneEmission ds e ps = error "Empty input state"
