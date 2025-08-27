{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Utils where

import Data.List.Extra (drop1)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

updateNthElement :: Int -> a -> [a] -> [a]
updateNthElement n x xs = let (as, bs) = splitAt n xs in as ++ x: drop1 bs

insertInNthPosition :: Int -> a -> [a] -> [a]
insertInNthPosition n x xs = let (as, bs) = splitAt n xs in as ++ x: bs

deleteNthElement :: Int -> [a] -> [a]
deleteNthElement n xs = let (as, bs) = splitAt n xs in as ++ drop1 bs
