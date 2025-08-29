{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module POS.HMM.Types where

import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Data.List (nub)

import POS.HMM.Matrix

data HMMDataT h e p = HMMDataT {
    startState :: Int,
    stopState :: Int,
    initialProbability :: p,
    hiddenStates :: V.Vector h,
    emissions :: V.Vector e,
    hidden2hidden :: VMatrix p,
    hidden2emission :: VMatrix p
    }

type HMMIndexedResultT p = ([Int], p)

data HMMResultT h p = HMMResultT {
    states :: [h],
    probability :: p
    } deriving (Show)

transition :: Num p => HMMDataT h e p -> Int -> Int -> p
transition d = get (hidden2hidden d)

emission :: Num p => HMMDataT h e p -> Int -> Int -> p
emission d = get (hidden2emission d)

emissionsToIndexes :: Eq e => HMMDataT h e p -> [e] -> [Int]
emissionsToIndexes d = map (fromMaybe (-1) . flip V.elemIndex (emissions d))

indexesToEmissions :: HMMDataT h e p -> [Int] -> [e]
indexesToEmissions d = map (emissions d V.!)

stateToIndexes :: Eq h => HMMDataT h e p -> [h] -> [Int]
stateToIndexes d = map (fromMaybe (-1) . flip V.elemIndex (hiddenStates d))

indexesToStates :: HMMDataT h e p -> [Int] -> [h]
indexesToStates d = map (hiddenStates d V.!)

hiddenStatesSize :: HMMDataT h e p -> Int
hiddenStatesSize d = V.length (hiddenStates d)

unpackResult :: HMMDataT h e p -> HMMIndexedResultT p -> HMMResultT h p
unpackResult d (ixhs, p) = HMMResultT (map (hiddenStates d V.!) ixhs) p