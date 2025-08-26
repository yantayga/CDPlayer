{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Common where

import CDDB.CDDB

import Editor.Command.Types
import Editor.Command.Settings

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState {settings = settings, cddb = emptyCDDB, currentRules = [], isNotSaved = True, currentTemplate = Nothing}

errTooManyArguments :: IO (Either String ProgramState)
errTooManyArguments = return $ Left "Too many arguments."