{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands (runMainCommand, initialProgramState, isNotSaved, settings)  where

import qualified Data.Map as M
import Data.List (isInfixOf)
import Text.Read (readEither)

import Editor.Command.Settings
import Editor.Command.Common
import Editor.Command.Types
import Editor.Command.Help
import Editor.Command.CDDB
import Editor.Command.Rules
import Editor.Command.Templates
import Editor.Command.Runner

commands :: CommandMap
commands = M.fromList [
        ("help",     CommandDef (cmdHelp commands) "This help."),
        ("new",      CommandDef cmdCreateEmptyCDDB "Create new database."),
        ("save",     CommandDef cmdSaveCDDB "Save database with optional file name."),
        ("load",     CommandDef cmdLoadCDDB "Load database with optional file name."),
        ("get",      CommandDef (runCommand getCommands) "Get objects field."),
        ("set",      CommandDef (runCommand setCommands) "Set objects field."),
        ("rule",     CommandDef (runCommand ruleCommands) "Manimulate one rule, my index."),
        ("rules",    CommandDef (runCommand rulesCommands) "Manimulate rules."),
        ("template", CommandDef (runCommand templateCommands) "Manipulate templates."),
        ("run",      CommandDef cmdRun "Run for syntactic tree."),
        ("dump",     CommandDef cmdDumpCDDB "Dump database."),
        ("quit",     CommandDef cmdQuit "Quit program.")
    ]

getCommands :: CommandMap
getCommands = M.fromList [
        ("help",     CommandDef (cmdHelp getCommands) "This help."),
        ("tree",     CommandDef (cmdGetField  $ makeGetter settings cddbTree) "Get current syntactic tree."),
        ("cddb",     CommandDef (runCommand getCDDBCommands) "Get database parameters."),
        ("settings", CommandDef (runCommand getSettingsCommands) "Get settings.")
    ]

setCommands :: CommandMap
setCommands = M.fromList [
        ("help",     CommandDef (cmdHelp setCommands) "This help."),
        ("tree",     CommandDef cmdSetTree "Set current syntactic tree."),
        ("cddb",     CommandDef (runCommand setCDDBCommands) "Set database fields."),
        ("settings", CommandDef (runCommand setSettingsCommands) "Set settings fields.")
    ]

getSettingsCommands :: CommandMap
getSettingsCommands = M.fromList [
        ("help",              CommandDef (cmdHelp getSettingsCommands) "This help."),
        ("historyFile",       CommandDef (cmdGetField $ makeGetter settings historyFile) "Get histroy file."),
        ("autoAddHistory",    CommandDef (cmdGetField $ makeGetter settings autoAddHistory) "Get enable/disable add command to history."),
        ("maxRecursionDepth", CommandDef (cmdGetField $ makeGetter settings maxRecursionDepth) "Get maximum recursion depth.")
    ]

setSettingsCommands :: CommandMap
setSettingsCommands = M.fromList [
        ("help",              CommandDef (cmdHelp setSettingsCommands) "This help."),
        ("historyFile",       CommandDef (cmdSetField $ makeSetter setSettings setHistoryFile settings) "Set histroy file."),
        ("autoAddHistory",    CommandDef (cmdSetField $ makeSetter setSettings setAutoAddHistory settings) "Enable/disable add command to history."),
        ("maxRecursionDepth", CommandDef (cmdSetField $ makeSetter setSettings setMaxRecursionDepth settings) "Set maximum recursion depth.")
    ]

runMainCommand :: Command
runMainCommand = runCommand commands

runCommand :: CommandMap -> Command
runCommand cmds cmd state =
    case cmd of
        [] -> return $ Left "Empty command"
        (cmdName: args) ->
            case M.lookup cmdName cmds of
                Nothing -> return $ Left $ "Command '" ++ cmdName ++ "' not found. Possible variants: " ++ findMostSimilar cmdName
                Just (CommandDef fn _) -> fn args state
    where
        findMostSimilar cmdName = unwords $ (filter . isInfixOf) cmdName $ M.keys cmds

cmdSetTree :: Command
cmdSetTree args state = let tree = (readEither $ unwords args) in
        case tree of
                Left err -> return $ Left err
                Right a -> return $ Right $ state {settings = (settings state) {cddbTree = Just a}}

setSettings :: ProgramState -> Settings -> ProgramState
setSettings state settings = state {settings = settings}

cmdQuit :: Command
cmdQuit = undefined
