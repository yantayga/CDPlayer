{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands (runMainCommand, initialProgramState, isNotSaved, settings)  where

import Data.Time
import qualified Data.Map as M
import Data.List (intercalate, isInfixOf)
import Data.Either.Extra (maybeToEither)
import Data.Aeson (encode, decode, toJSON, fromJSON)
import qualified Data.ByteString.Lazy as B
import Text.Read
import Data.UUID (fromString)
import Data.Maybe (mapMaybe)

import Control.Monad.Catch (catch, SomeException)
import System.IO

import CDDB.Types
import CDDB.Rules
import CDDB.Runner
import CDDB.CDDB
import CDDB.Utils
import CDDB.Tree.Syntax

import Editor.Settings

data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        currentRules :: [(RuleId, Rule)],
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name,
        currentRule :: Maybe Rule
    }
    deriving (Show)

type Arguments = [String]

type Command = Arguments -> ProgramState -> IO (Either String ProgramState)

type HelpString = String

data CommandDef = CommandDef Command HelpString

type CommandMap = M.Map String CommandDef

commands :: CommandMap
commands = M.fromList [
        ("new",  CommandDef cmdCreateEmptyCDDB "Create new database."),
        ("save", CommandDef cmdSaveCDDB "Save database with optional file name."),
        ("load", CommandDef cmdLoadCDDB "Load database with optional file name."),
        ("help", CommandDef (cmdHelp commands) "This help."),
        ("get", CommandDef (runCommand getCommands) "Get objects field."),
        ("set", CommandDef (runCommand setCommands) "Set objects field."),
        ("rule", CommandDef (runCommand ruleCommands) ""),
        ("quit", CommandDef cmdQuit "Quit program.")
    ]

ruleCommands :: CommandMap
ruleCommands = M.fromList [
        ("help", CommandDef (cmdHelp ruleCommands) "This help."),
        ("find", CommandDef cmdFindRules "Filter rules by ids."),
        ("filter", CommandDef cmdFilterRules "Filter rules by syntactic tree.")
    ]

getCommands :: CommandMap
getCommands = M.fromList [
        ("help", CommandDef (cmdHelp getCommands) "This help."),
        ("tree", CommandDef (cmdGetField  $ makeGetter settings cddbTree) "Get current syntactic tree."),
        ("cddb", CommandDef (runCommand getCDDBCommands) "Get database parameters."),
        ("settings", CommandDef (runCommand getSettingsCommands) "Get settings.")
    ]

setCommands :: CommandMap
setCommands = M.fromList [
        ("help", CommandDef (cmdHelp setCommands) "This help."),
        ("tree", CommandDef cmdSetTree "Set current syntactic tree."),
        ("cddb", CommandDef (runCommand setCDDBCommands) "Set database fields."),
        ("settings", CommandDef (runCommand setSettingsCommands) "Set settings fields.")
    ]

getCDDBCommands :: CommandMap
getCDDBCommands = M.fromList [
        ("name", CommandDef (cmdGetField $ makeGetter cddb name) "Get database name."),
        ("comment", CommandDef (cmdGetField $ makeGetter cddb comment) "Get database comment."),
        ("version", CommandDef (cmdGetField $ makeGetter cddb version) "Get database version."),
        ("date", CommandDef (cmdGetField $ makeGetter cddb date) "Get database date."),
        ("filename", CommandDef (cmdGetField $ makeGetter settings cddbFileName) "Get database filename."),
        ("help", CommandDef (cmdHelp getCDDBCommands) "This help.")
    ]

setCDDBCommands :: CommandMap
setCDDBCommands = M.fromList [
        ("name", CommandDef (cmdSetField $ makeSetter setCDDB setName cddb) "Set database name."),
        ("comment", CommandDef (cmdSetField $ makeSetter setCDDB setComment cddb) "Set database comment."),
        ("version", CommandDef (cmdSetField $ makeSetter setCDDB setVersion cddb) "Set database version."),
        ("date", CommandDef (cmdSetField $ makeSetter setCDDB setDate cddb) "Set database date."),
        ("help", CommandDef (cmdHelp setCDDBCommands) "This help.")
    ]

getSettingsCommands :: CommandMap
getSettingsCommands = M.fromList [
        ("historyFile", CommandDef (cmdGetField $ makeGetter settings historyFile) "Get histroy file."),
        ("autoAddHistory", CommandDef (cmdGetField $ makeGetter settings autoAddHistory) "Get enable/disable add command to history."),
        ("help", CommandDef (cmdHelp getSettingsCommands) "This help.")
    ]

setSettingsCommands :: CommandMap
setSettingsCommands = M.fromList [
        ("historyFile", CommandDef (cmdSetField $ makeSetter setSettings setHistoryFile settings) "Set histroy file."),
        ("autoAddHistory", CommandDef (cmdSetField $ makeSetter setSettings setAutoAddHistory settings) "Enable/disable add command to history."),
        ("help", CommandDef (cmdHelp setSettingsCommands) "This help.")
    ]

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState {settings = settings, cddb = emptyCDDB, currentRules = [], isNotSaved = True, currentTemplate = Nothing, currentRule = Nothing}

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

cmdFilterRules :: Command
cmdFilterRules args state = printAndUpdateCurrentRules (boundRuleDesc filter) (mapSnd fst) rulesFound state
    where
        filter = read $ unwords args
        rulesFound = M.toList $ matchRulesAndFindPaths filter (rules $ cddb state)

cmdFindRules :: Command
cmdFindRules args state = printAndUpdateCurrentRules ruleDesc id rulesFound state
    where
        ids = mapMaybe fromString args
--        rulesFound = mapMaybe (findCDDBRuleById $ cddb state) ids
        rulesFound = M.toList $ rules $ cddb state

printAndUpdateCurrentRules pf pre rulesFound state =  if null rulesFound
    then return $ Left "No rules found"
    else do
        mapM_ (putStrLn . pf) rulesFound
        return $ Right state {currentRules = map pre rulesFound}

cmdSetTree :: Command
cmdSetTree args state = let tree = (readEither $ unwords args) in
        case tree of
                Left err -> return $ Left err
                Right a -> return $ Right $ state {settings = (settings state) {cddbTree = Just a}}

cmdGetField :: Show b => (ProgramState -> b) -> Command
cmdGetField accessor [] state = return $ Left $ show $ accessor state
cmdGetField _ _ _ = return $ Left "Too many arguments"

cmdSetField :: Read b => (ProgramState -> b -> ProgramState) -> Command
cmdSetField setter [val] state = let v = readEither val in
        case v of
                Left err -> return $ Left err
                Right a -> return $ Right $ setter state a
cmdSetField _ [] _ = return $ Left "Not enough arguments"
cmdSetField _ _ _ = return $ Left "Too many arguments"

makeGetter :: (a -> b) -> (b -> c) -> (a -> c)
makeGetter = flip (.)

makeSetter :: (a -> b -> a) -> (b -> c -> b) -> (a -> b) -> (a -> c -> a)
makeSetter outer inner accessor a c = outer a $ inner (accessor a) c

setCDDB :: ProgramState -> CDDB -> ProgramState
setCDDB state cddb = state {cddb = cddb}

setName :: CDDB -> String -> CDDB
setName cddb name = cddb {name = name}

setComment :: CDDB -> String -> CDDB
setComment cddb comment = cddb {comment = comment}

setVersion :: CDDB -> Integer -> CDDB
setVersion cddb version = cddb {version = version}

setDate :: CDDB -> UTCTime -> CDDB
setDate cddb date = cddb {date = date}

setSettings :: ProgramState -> Settings -> ProgramState
setSettings state settings = state {settings = settings}

setHistoryFile :: Settings -> String -> Settings
setHistoryFile settings historyFile = settings {historyFile = historyFile}

setAutoAddHistory :: Settings -> Bool -> Settings
setAutoAddHistory settings autoAddHistory = settings {autoAddHistory = autoAddHistory}

cmdQuit :: Command
cmdQuit = undefined

cmdHelp :: CommandMap -> Command
cmdHelp cmds args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds
    where
        addCommandHelp names key (CommandDef _ def) acc = if null names || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB args state = return $ Right $ initialProgramState $ settings state

cmdSaveCDDB :: Command
cmdSaveCDDB args state = do
    today <- getCurrentTime
    case extractFileName args state of
        Left errMsg -> return $ Left errMsg
        Right fn -> let updatedCDDB = (cddb state) {date = today} in do
            B.writeFile fn $ encode (toJSON updatedCDDB)
            return $ Right $ updateStateWithDB state updatedCDDB fn

cmdLoadCDDB :: Command
cmdLoadCDDB args state = do
    case extractFileName args state of
        Left errMsg -> return $ Left errMsg
        Right fn -> do
            handle <- openFile fn ReadMode
            fileContent <- B.hGetContents handle
            case (decode :: B.ByteString -> Maybe CDDB) fileContent of
                Nothing -> return $ Left "Error reading file "
                Just cddb -> return $ Right $ updateStateWithDB state cddb fn

updateStateWithDB :: ProgramState -> CDDB -> FilePath -> ProgramState
updateStateWithDB state cddb fn = state {cddb = cddb, isNotSaved = False, settings = (settings state) {cddbFileName = Just fn}}

extractFileName :: Arguments -> ProgramState -> Either String String
extractFileName [] state = maybeToEither "File name is not specified" $ cddbFileName $ settings state
extractFileName [s] _ = Right s
extractFileName _ _ = Left "Should be only one file name"
