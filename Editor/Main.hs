{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import System.Console.Haskeline
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (catch, SomeException)
import System.IO

import Editor.Settings
import Editor.Commands

agreedNotToSave :: InputT IO (Bool)
agreedNotToSave = do
    answer <- getInputChar "CDDB is not saved. Dou you really want to quit (y/N)?"
    return $ answer == Just 'N' || answer == Just 'n'

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
                Just input -> (flip catch) exceptonHandler $ do
                    res <- liftIO $ runCommand commands (words input) state
                    case res of
                        Left errorMessage -> outputStrLn errorMessage >> loop state
                        Right state' -> loop state'

        exceptonHandler :: SomeException -> InputT IO ()
        exceptonHandler ex = outputStrLn $ "Exception" ++ show ex
