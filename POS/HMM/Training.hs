{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module POS.HMM.Training where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List.Extra (drop1)

import POS.HMM.Types
import POS.HMM.Matrix
import POS.Conluu

collectHidden :: Matrix m Double => m -> [Int] -> m
collectHidden m hs = collect m $ zip hs (drop1 hs)

collectEmissions :: Matrix m Double => m -> [(Int, Int)] -> m
collectEmissions = collect

collect :: Matrix m Double => m -> [(Int, Int)] -> m
collect = foldl updateAt

updateAt :: Matrix m Double => m -> (Int, Int) -> m
updateAt m (ix1, ix2) = set m ix1 ix2 (get m ix1 ix2 + 1.0)

normalize :: (Fractional p, Num p) => Matrix m p => p -> m -> m
normalize smoothingFactor m = imap normalizeItem m
    where
        prs = perRowSums m
        normalizeItem r c v = (v + smoothingFactor) / (prs V.! r + smoothingFactor)

collectHiddensConluu :: (Fractional p, Integral p, Matrix m Double) => ConluuData -> m
collectHiddensConluu d = foldl collectHidden (empty hiddenSize hiddenSize 0.0) hiddenStates
    where
        hiddenSize = M.size $ uPOSTags d
        hiddenStates = map (map oposTag . items) $ sentences d

collectEmissionsConluu :: (Fractional p, Integral p, Matrix m Double) => ConluuData -> m
collectEmissionsConluu d = foldl collectEmissions (empty hiddenSize emissionsSize 0.0) states
    where
        hiddenSize = M.size $ uPOSTags d
        emissionsSize = M.size $ fullWords d
        states = map ((\x -> zip (map oposTag x) (map word x)). items) $ sentences d

