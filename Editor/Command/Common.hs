{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Common where

import Text.Read (readEither)

import CDDB.CDDB

import Editor.Command.Types
import Editor.Command.Settings

cmdGetField :: Show b => (ProgramState -> b) -> Command
cmdGetField accessor [] state = return $ Left $ show $ accessor state
cmdGetField _ _ _ = errTooManyArguments

cmdSetField :: Read b => (ProgramState -> b -> ProgramState) -> Command
cmdSetField setter [val] state = let v = readEither val in
        case v of
                Left err -> return $ Left err
                Right a -> return $ Right $ setter state a
cmdSetField _ [] _ = errTooManyArguments
cmdSetField _ _ _ = return $ Left "Too many arguments"

makeGetter :: (a -> b) -> (b -> c) -> (a -> c)
makeGetter = flip (.)

makeSetter :: (a -> b -> a) -> (b -> c -> b) -> (a -> b) -> (a -> c -> a)
makeSetter outer inner accessor a c = outer a $ inner (accessor a) c

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState {settings = settings, cddb = emptyCDDB, currentRules = [], isNotSaved = True, currentTemplate = Nothing}

errTooManyArguments :: IO (Either String ProgramState)
errTooManyArguments = return $ Left "Too many arguments."