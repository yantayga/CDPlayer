{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Catch (catch, SomeException)

import Editor.Commands
import Editor.Command.Settings

-- TODO: Use agreedNotToSave
agreedNotToSave :: InputT IO Bool
agreedNotToSave = do
    answer <- getInputChar "CDDB is not saved. Dou you really want to quit (y/N)?"
    return $ answer == Just 'N' || answer == Just 'n'

main :: IO ()
main = do
        startSettings <- readSettings
        let state = initialProgramState startSettings in do
            runInputTWithPrefs (haskelinePrefsFromSettings startSettings) (haskelineSettionsFromSettings startSettings) $ loop state
    where
        loop state = do
            minput <- getInputLine "CDDB> "
            case minput of
                Nothing -> return ()
                Just "quit" -> do
                        liftIO $ writeSettings $ settings state
                        return ()
                Just input -> flip catch exceptonHandler $ do
                    res <- liftIO $ runMainCommand (words input) state
                    case res of
                        Left errorMessage -> outputStrLn errorMessage >> loop state
                        Right state' -> loop state'

        exceptonHandler :: SomeException -> InputT IO ()
        exceptonHandler ex = outputStrLn $ "Exception" ++ show ex
