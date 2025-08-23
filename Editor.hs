{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor where

import System.Console.Haskeline
import Control.Monad (unless)
import System.IO

import Editor.Settings
import Editor.Commands

agreedNotToSave :: InputT IO (Bool)
agreedNotToSave = do
    answer <- getInputChar "CDDB is not saved. Dou you really want to quit (y/N)?"
    return $ answer == Just 'N'

main :: IO ()
main = do
        settings <- readSettings
        runInputTWithPrefs (haskelinePrefsFromSettings settings) (haskelineSettionsFromSettings settings) $ loop initialProgramState
        writeSettings settings
    where
        loop :: ProgramState -> InputT IO ()
        loop state = do
            minput <- getInputLine "CDDB> "
            case minput of
                Nothing -> return ()
                Just "quit" -> do
                    if isNotSaved state then do
                        exiting <- agreedNotToSave
                        if exiting then return() else loop state
                    else return ()
                Just input -> do
                    case runCommand input state of
                        Left errorMessage -> outputStrLn errorMessage >> loop state
                        Right state' -> loop state'


