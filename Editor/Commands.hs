{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands where

import CDDB.Types
import CDDB.Process

import System.Console.Haskeline

import qualified Data.Map as M
import Data.List (intercalate, isInfixOf)

data ProgramState = ProgramState {
        cddb :: CDDB,
        cddbFileName :: String,
        isNotSaved :: Bool,
        currentRule :: Maybe Rule
    }

type Arguments = [String]

type Command = Arguments -> ProgramState -> Either String ProgramState

type HelpString = String

data CommandDef = CommandDef Command HelpString

commands :: M.Map String CommandDef
commands = M.fromList [
        ("new",  CommandDef cmdCreateEmptyCDDB "Create new database."),
        ("save", CommandDef cmdSaveCDDB "Save database with optional file name."),
        ("load", CommandDef cmdLoadCDDB "Load database with optional file name."),
        ("help", CommandDef cmdHelp "This help."),
        ("test", CommandDef cmdTestErrMsg "Show test error message with arguments.")
    ]

initialProgramState :: ProgramState
initialProgramState = ProgramState {cddb = emptyCDDB, cddbFileName = "", isNotSaved = True, currentRule = Nothing}

runCommand :: String -> ProgramState -> Either String ProgramState
runCommand cmd state =
    case words cmd of
        [] -> Left "Empty command"
        (cmdName: args) ->
            case M.lookup cmdName commands of
                Nothing -> Left $ "Command '" ++ cmdName ++ "' not found. Possible variants: " ++ intercalate " " (findMostSimilar cmdName)
                Just (CommandDef fn _) -> fn args state

findMostSimilar :: String -> [String]
findMostSimilar cmdname = (filter . isInfixOf) cmdname $ M.keys commands

cmdTestErrMsg :: Command
cmdTestErrMsg args _= Left $ "TEST ERROR MESSAGE: " ++ intercalate " " args

cmdHelp :: Command
cmdHelp args _ = Left $ M.foldrWithKey (addCommadHelp args) "Commands:\n" commands
    where
        addCommadHelp names key (CommandDef _ def) acc = acc ++ "\n" ++ key ++ "\n\t" ++ def

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB args state = Right initialProgramState

cmdSaveCDDB :: Command
cmdSaveCDDB args state = undefined

cmdLoadCDDB :: Command
cmdLoadCDDB args state = undefined

