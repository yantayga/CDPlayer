{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands where

import CDDB.Types
import CDDB.Process

import qualified Data.Map as M
import Data.List (intercalate)

data ProgramState = ProgramState {
        cddb :: CDDB,
        currentRule :: Maybe Rule,
        isNotSaved :: Bool
    }

type Arguments = [String]

type Command = Arguments -> ProgramState -> Either String ProgramState

commands :: M.Map String Command
commands = M.fromList [
        ("test", cmdTestErMsg),
        ("new", cmdCreateEmptyCDDB)
    ]

initialProgramState :: ProgramState
initialProgramState = ProgramState emptyCDDB Nothing True

runCommand :: String -> ProgramState -> Either String ProgramState
runCommand cmd state =
    case words cmd of
        [] -> Left "Empty command"
        (cmdName: args) ->
            case M.lookup cmdName commands of
                Nothing -> Left $ "Command '" ++ cmdName ++ "' not found"
                Just fn -> fn args state

cmdTestErMsg :: Command
cmdTestErMsg args _= Left $ "TEST ERROR MESSAGE: " ++ intercalate " " args

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB args state = Right $ state { cddb = emptyCDDB, currentRule = Nothing, isNotSaved = True}

