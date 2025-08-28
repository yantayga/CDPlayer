{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}

module POS.HMMData where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.List (nub)

data HMMData h e = HMMData { 
    hiddenStates :: HiddenStates h,
    transitions :: TransitionsData h,
    emissions :: EmissionsData h e
    }

newtype HiddenStates h = HiddenStates [h] deriving (Show)

newtype TransitionsData h = TransitionsData (M.Map (h, h) Probability)

newtype EmissionsData h e = EmissionsData (M.Map (h, e) Probability)

type Probability = Double

transition :: Ord h => TransitionsData h -> h -> h -> Probability
transition (TransitionsData tm) node1 node2 = fromMaybe 0 $ M.lookup (node1, node2) tm

emission :: Ord h => Ord e => EmissionsData h e -> h -> e -> Probability
emission (EmissionsData em) node1 node2 = fromMaybe 0 $ M.lookup (node1, node2) em

hiddenStatesFromTransitionsData :: Eq h => TransitionsData h -> HiddenStates h
hiddenStatesFromTransitionsData (TransitionsData m) = HiddenStates $ nub $ map fst $ M.keys m
