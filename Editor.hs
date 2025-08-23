{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor where

import Control.Monad (unless)
import System.IO

main :: IO ()
main = do
  input <- readInput

  unless (input == ":quit")
       $ printResponse (evalCommand input)
      >> main


readInput :: IO String
readInput = putStr "CDDB> "
     >> hFlush stdout
     >> getLine


evalCommand :: String -> String
evalCommand input = input

printResponse :: String -> IO ()
printResponse = putStrLn