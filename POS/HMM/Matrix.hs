{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, AllowAmbiguousTypes #-}

module POS.HMM.Matrix where

import qualified Data.Vector as V

class Num p => Matrix m p where
    get :: m -> Int -> Int -> p
    set :: m -> Int -> Int -> p -> m
    row :: m -> Int -> V.Vector p
    empty :: Int -> Int -> p -> m
    imap :: (Int -> Int -> p -> p) -> m -> m
    perRowSums :: m -> V.Vector p

data VMatrix p = VMatrix {
    vector :: V.Vector p,
    numRows :: Int,
    numCols :: Int
    }

instance Num p => Matrix (VMatrix p) p where
    get m r c = vector m V.! (r * numCols m + c)
    set m r c v = m {vector = vector m V.// [(r * numCols m + c, v)]}
    row m r = V.slice (r * numCols m) (numCols m) (vector m)
    empty r c v = VMatrix {vector = V.replicate (r * c) v, numCols = c, numRows = r}
    imap f m = m {vector = V.imap f' (vector m)}
        where f' ix v = let (r, c) = ix `divMod` numCols m in f r c v
    perRowSums m = V.fromList $ map rowSum [0 .. numRows m - 1]
        where rowSum rowIx = V.sum $ row m rowIx
