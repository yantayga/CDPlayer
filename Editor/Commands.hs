{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands (runMainCommand, initialProgramState, isNotSaved)  where

import CDDB.Types
import CDDB.Process
import Editor.Settings

import Data.Time
import qualified Data.Map as M
import Data.List (intercalate, isInfixOf)
import Data.Either.Extra (maybeToEither)
import Data.Aeson (encode, decode, toJSON, fromJSON)
import qualified Data.ByteString.Lazy as B

import System.IO

data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        cddbFileName :: Maybe String,
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name,
        currentRule :: Maybe Rule
    }

type Arguments = [String]

type Command = Arguments -> ProgramState -> IO (Either String ProgramState)

type HelpString = String

data CommandDef = CommandDef Command HelpString (Maybe (CommandMap))

type CommandMap = M.Map String CommandDef

commands :: CommandMap
commands = M.fromList [
        ("new",  CommandDef cmdCreateEmptyCDDB "Create new database." Nothing),
        ("save", CommandDef cmdSaveCDDB "Save database with optional file name." Nothing),
        ("load", CommandDef cmdLoadCDDB "Load database with optional file name." Nothing),
        ("help", CommandDef (cmdHelp commands) "This help." Nothing),
        ("get", CommandDef (runCommand getCommands) "Set objects field." Nothing),
        ("set", CommandDef (runCommand setCommands) "Get objects field.." Nothing),
        ("quit", CommandDef cmdQuit "Quit program." Nothing)
    ]

getCommands :: CommandMap
getCommands = M.fromList [
        ("help", CommandDef (cmdHelp getCommands) "This help." Nothing),
        ("cddb", CommandDef (runCommand getCDDBCommands) "Get database name." Nothing),
        ("settings", CommandDef (runCommand getSettingsCommands) "Get database comment." Nothing)
    ]

setCommands :: CommandMap
setCommands = M.fromList [
        ("help", CommandDef (cmdHelp setCommands) "This help." Nothing),
        ("cddb", CommandDef (runCommand setCDDBCommands) "Set database fields." Nothing),
        ("settings", CommandDef (runCommand setSettingsCommands) "Set settings fields." Nothing)
    ]

getCDDBCommands :: CommandMap
getCDDBCommands = M.fromList [
        ("help", CommandDef (cmdHelp getCDDBCommands) "This help." Nothing),
        ("name", CommandDef (cmdGetField cddb name) "Get database name." Nothing),
        ("comment", CommandDef (cmdGetField cddb comment) "Get database comment." Nothing),
        ("version", CommandDef (cmdGetField cddb version) "Get database version." Nothing),
        ("date", CommandDef (cmdGetField cddb date) "Get database date." Nothing)
    ]

setCDDBCommands :: CommandMap
setCDDBCommands = M.fromList [
        ("help", CommandDef (cmdHelp setCDDBCommands) "This help." Nothing),
        ("name", CommandDef (cmdSetField set_cddb cddb set_name) "Set database name." Nothing),
        ("comment", CommandDef (cmdSetField set_cddb cddb set_comment) "Set database comment." Nothing),
        ("version", CommandDef (cmdSetField set_cddb cddb set_version) "Set database version." Nothing),
        ("date", CommandDef (cmdSetField set_cddb cddb set_date) "Set database date." Nothing)
    ]

getSettingsCommands :: CommandMap
getSettingsCommands = M.fromList [
        ("help", CommandDef (cmdHelp getSettingsCommands) "This help." Nothing),
        ("historyFile", CommandDef (cmdGetField settings historyFile) "Get histroy file." Nothing),
        ("autoAddHistory", CommandDef (cmdGetField settings autoAddHistory) "Get enable/disable add command to history." Nothing)
    ]

setSettingsCommands :: CommandMap
setSettingsCommands = M.fromList [
        ("help", CommandDef (cmdHelp setSettingsCommands) "This help." Nothing),
        ("historyFile", CommandDef (cmdSetField set_settings settings set_historyFile) "Set histroy file." Nothing),
        ("autoAddHistory", CommandDef (cmdSetField set_settings settings set_autoAddHistory) "Eable/disable add command to history." Nothing)
    ]

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState {settings = settings, cddb = emptyCDDB, cddbFileName = Nothing, isNotSaved = True, currentTemplate = Nothing, currentRule = Nothing}

runMainCommand :: Command
runMainCommand = runCommand commands

runCommand :: CommandMap -> Command
runCommand cmds cmd state =
    case cmd of
        [] -> return $ Left "Empty command"
        (cmdName: args) ->
            case M.lookup cmdName cmds of
                Nothing -> return $ Left $ "Command '" ++ cmdName ++ "' not found. Possible variants: " ++ findMostSimilar cmdName
                Just (CommandDef fn _ subs) -> fn args state
    where
        findMostSimilar cmdName = intercalate " " $ (filter . isInfixOf) cmdName $ M.keys cmds

cmdGetField :: Show b => (ProgramState -> a) -> (a -> b) -> Command
cmdGetField state_accessor object_accessor [] state = return $ Left $ show $ object_accessor $ state_accessor state
cmdGetField _ _ _ _ = return $ Left "Too many arguments"

cmdSetField :: Read b => (ProgramState -> a -> ProgramState) -> (ProgramState -> a) -> (a -> b -> a) -> Command
cmdSetField state_setter state_accessor object_setter (val:[]) state = return $ Right $ state_setter state (object_setter (state_accessor state) (read val))
cmdSetField _ _ _ [] _ = return $ Left "Not enough arguments"
cmdSetField _ _ _ _ _ = return $ Left "Too many arguments"

set_cddb :: ProgramState -> CDDB -> ProgramState
set_cddb state cddb = state {cddb = cddb}

set_name :: CDDB -> String -> CDDB
set_name cddb name = cddb {name = name}

set_comment :: CDDB -> String -> CDDB
set_comment cddb comment = cddb {comment = comment}

set_version :: CDDB -> Integer -> CDDB
set_version cddb version = cddb {version = version}

set_date :: CDDB -> UTCTime -> CDDB
set_date cddb date = cddb {date = date}

set_settings :: ProgramState -> Settings -> ProgramState
set_settings state settings = state {settings = settings}

set_historyFile :: Settings -> String -> Settings
set_historyFile settings historyFile = settings {historyFile = historyFile}

set_autoAddHistory :: Settings -> Bool -> Settings
set_autoAddHistory settings autoAddHistory = settings {autoAddHistory = autoAddHistory}

--cmdGetSettingsField :: Show a => (Settings -> a) -> Command
--cmdGetSettingsField accessor [] state = return $ Left $ show $ accessor $ cddb state
--cmdGetSettingsField _ _ _ = return $ Left "Too many arguments"

cmdQuit :: Command
cmdQuit = undefined

cmdHelp :: CommandMap -> Command
cmdHelp cmds args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds
    where
        addCommandHelp names key (CommandDef _ def subs) acc = if names == [] || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB args state = return $ Right $ initialProgramState $ settings state

cmdSaveCDDB :: Command
cmdSaveCDDB args state = do
    today <- getCurrentTime
    case extractFileName args state of
        Left errMsg -> return $ Left errMsg
        Right fn -> let updatedCDDB = (cddb state) {date = today} in do
            B.writeFile fn $ encode (toJSON $ updatedCDDB)
            return $ Right state {cddb = updatedCDDB, isNotSaved = False}

cmdLoadCDDB :: Command
cmdLoadCDDB args state = do
    case extractFileName args state of
        Left errMsg -> return $ Left errMsg
        Right fn -> do
            handle <- openFile fn ReadMode
            fileContent <- B.hGetContents handle
            case (decode :: B.ByteString -> Maybe CDDB) fileContent of
                Nothing -> return $ Left "Error reading file "
                Just s -> return $ Right state {isNotSaved = False}

extractFileName :: Arguments -> ProgramState -> Either String String
extractFileName [] state = maybeToEither "File name is not specified" $ cddbFileName state
extractFileName (s:[]) _ = Right s
extractFileName _ _ = Left "Should be only one file name"
