{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands (runMainCommand, initialProgramState, isNotSaved, settings)  where

import qualified Data.Map as M
import Data.List (isInfixOf)
import Text.Read

import CDDB.CDDB

import Editor.Command.Settings
import Editor.Command.Common
import Editor.Command.Types
import Editor.Command.CDDB
import Editor.Command.Rules
import Editor.Command.Runner

commands :: CommandMap
commands = M.fromList [
        ("help",  CommandDef (cmdHelp commands) "This help."),
        ("new",   CommandDef cmdCreateEmptyCDDB "Create new database."),
        ("save",  CommandDef cmdSaveCDDB "Save database with optional file name."),
        ("load",  CommandDef cmdLoadCDDB "Load database with optional file name."),
        ("get",   CommandDef (runCommand getCommands) "Get objects field."),
        ("set",   CommandDef (runCommand setCommands) "Set objects field."),
        ("rule",  CommandDef (runCommand ruleCommands) "Manimulate one rule, my index"),
        ("rules", CommandDef (runCommand rulesCommands) "Manimulate rules"),
        ("run",   CommandDef cmdRun "Run for syntactic tree."),
        ("dump",  CommandDef cmdDumpCDDB "Dump database."),
        ("quit",  CommandDef cmdQuit "Quit program.")
    ]

ruleCommands :: CommandMap
ruleCommands = M.fromList [
        ("help",    CommandDef (cmdHelp ruleCommands) "This help."),
        ("add",     CommandDef cmdAddRule "Add rule to current rules."),
        ("write",   CommandDef (cmdWriteRules filterByN) "Add/update rule #arg to cddb."),
        ("renew",   CommandDef (cmdRenewRules filterByN) "Regenerate rule #arg ids."),
        ("delete",  CommandDef (cmdDeleteRules filterByN) "Delete rule #arg from the current rules"),
        ("wipe",    CommandDef (cmdWipeRules filterByN) "Delete rule #arg with id from CDDB.")
    ]

rulesCommands :: CommandMap
rulesCommands = M.fromList [
        ("help",    CommandDef (cmdHelp rulesCommands) "This help."),
        ("show",    CommandDef cmdShowRules "Show current rules."),
        ("clear",   CommandDef (cmdDeleteRules useAll) "Clear current rules."),
        ("write",   CommandDef (cmdWriteRules useAll)"Add/update current rules to cddb."),
        ("renew",   CommandDef (cmdRenewRules useAll) "Regenerate rules ids."),
        ("find",    CommandDef cmdFindRules "Find rules by ids."),
        ("filter",  CommandDef cmdFilterRules "Find rules by syntactic tree.")
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

getCDDBCommands :: CommandMap
getCDDBCommands = M.fromList [
        ("help",     CommandDef (cmdHelp getCDDBCommands) "This help."),
        ("name",     CommandDef (cmdGetField $ makeGetter cddb name) "Get database name."),
        ("comment",  CommandDef (cmdGetField $ makeGetter cddb comment) "Get database comment."),
        ("version",  CommandDef (cmdGetField $ makeGetter cddb version) "Get database version."),
        ("date",     CommandDef (cmdGetField $ makeGetter cddb date) "Get database date."),
        ("filename", CommandDef (cmdGetField $ makeGetter settings cddbFileName) "Get database filename.")
    ]

setCDDBCommands :: CommandMap
setCDDBCommands = M.fromList [
        ("help",    CommandDef (cmdHelp setCDDBCommands) "This help."),
        ("name",    CommandDef (cmdSetField $ makeSetter setCDDB setName cddb) "Set database name."),
        ("comment", CommandDef (cmdSetField $ makeSetter setCDDB setComment cddb) "Set database comment."),
        ("version", CommandDef (cmdSetField $ makeSetter setCDDB setVersion cddb) "Set database version."),
        ("date",    CommandDef (cmdSetField $ makeSetter setCDDB setDate cddb) "Set database date.")
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

setSettings :: ProgramState -> Settings -> ProgramState
setSettings state settings = state {settings = settings}

cmdQuit :: Command
cmdQuit = undefined

cmdHelp :: CommandMap -> Command
cmdHelp cmds args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds
    where
        addCommandHelp names key (CommandDef _ def) acc = if null names || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""
