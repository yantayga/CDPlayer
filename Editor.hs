{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor where

import System.Console.Haskeline
import Control.Monad (unless)
import System.IO

import Editor.Settings

main :: IO ()
main = do
        settings <- readSettings
        runInputTWithPrefs (haskelinePrefsFromSettings settings) (haskelineSettionsFromSettings settings) loop
        writeSettings settings
    where
        loop :: InputT IO ()
        loop = do
            minput <- getInputLine "CDDB> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just input -> do outputStrLn $ "Input was: " ++ input
                                 loop
                                
