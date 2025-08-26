{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Commands (runMainCommand, initialProgramState, isNotSaved, settings)  where

import Data.Time
import qualified Data.Map as M
import Data.List (isInfixOf)
import Data.Either.Extra (maybeToEither)
import Data.Aeson (encode, decode, toJSON)
import qualified Data.ByteString.Lazy as B
import Text.Read
import Data.UUID (fromString)
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Data.UUID.V1

import Control.Monad (replicateM)
import System.IO

import CDDB.Types
import CDDB.Rules
import CDDB.CDDB
import CDDB.Utils
import CDDB.Runner

import Editor.Settings

data ProgramState = ProgramState {
        settings :: Settings,
        cddb :: CDDB,
        currentRules :: [(RuleId, Rule)],
        isNotSaved :: Bool,
        currentTemplate :: Maybe Name
    }
    deriving (Show)

type Arguments = [String]

type Command = Arguments -> ProgramState -> IO (Either String ProgramState)

type HelpString = String

data CommandDef = CommandDef Command HelpString

type CommandMap = M.Map String CommandDef

commands :: CommandMap
commands = M.fromList [
        ("new",   CommandDef cmdCreateEmptyCDDB "Create new database."),
        ("save",  CommandDef cmdSaveCDDB "Save database with optional file name."),
        ("load",  CommandDef cmdLoadCDDB "Load database with optional file name."),
        ("help",  CommandDef (cmdHelp commands) "This help."),
        ("get",   CommandDef (runCommand getCommands) "Get objects field."),
        ("set",   CommandDef (runCommand setCommands) "Set objects field."),
        ("rules", CommandDef (runCommand ruleCommands) "manimulate rules"),
        ("run",   CommandDef cmdRun "Run for syntactic tree."),
        ("dump",  CommandDef cmdDumpCDDB "Dump database."),
        ("quit",  CommandDef cmdQuit "Quit program.")
    ]

ruleCommands :: CommandMap
ruleCommands = M.fromList [
        ("help",    CommandDef (cmdHelp ruleCommands) "This help."),
        ("show",    CommandDef cmdShowRules "Show current rules."),
        ("clear",   CommandDef cmdClearRules "Clear current rules."),
        ("new",     CommandDef cmdNewRule "Set current rules to a new one."),
        ("add",     CommandDef cmdAddRule "Add rule to current rules."),
        ("write",   CommandDef cmdWriteRules "Add/update current rules to cddb."),
        ("renew",   CommandDef cmdRenewRules "Regenerate rules ids."),
        ("delete",  CommandDef cmdDeleteRule "Delete rule from the current rules"),
        ("wipe",    CommandDef cmdWipeRule "Delete rule with id from CDDB."),
        ("find",    CommandDef cmdFindRules "Filter rules by ids."),
        ("filter",  CommandDef cmdFilterRules "Filter rules by syntactic tree.")
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
        ("name",     CommandDef (cmdGetField $ makeGetter cddb name) "Get database name."),
        ("comment",  CommandDef (cmdGetField $ makeGetter cddb comment) "Get database comment."),
        ("version",  CommandDef (cmdGetField $ makeGetter cddb version) "Get database version."),
        ("date",     CommandDef (cmdGetField $ makeGetter cddb date) "Get database date."),
        ("filename", CommandDef (cmdGetField $ makeGetter settings cddbFileName) "Get database filename."),
        ("help",     CommandDef (cmdHelp getCDDBCommands) "This help.")
    ]

setCDDBCommands :: CommandMap
setCDDBCommands = M.fromList [
        ("name",    CommandDef (cmdSetField $ makeSetter setCDDB setName cddb) "Set database name."),
        ("comment", CommandDef (cmdSetField $ makeSetter setCDDB setComment cddb) "Set database comment."),
        ("version", CommandDef (cmdSetField $ makeSetter setCDDB setVersion cddb) "Set database version."),
        ("date",    CommandDef (cmdSetField $ makeSetter setCDDB setDate cddb) "Set database date."),
        ("help",    CommandDef (cmdHelp setCDDBCommands) "This help.")
    ]

getSettingsCommands :: CommandMap
getSettingsCommands = M.fromList [
        ("historyFile",       CommandDef (cmdGetField $ makeGetter settings historyFile) "Get histroy file."),
        ("autoAddHistory",    CommandDef (cmdGetField $ makeGetter settings autoAddHistory) "Get enable/disable add command to history."),
        ("maxRecursionDepth", CommandDef (cmdGetField $ makeGetter settings maxRecursionDepth) "Get maximum recursion depth."),
        ("help",              CommandDef (cmdHelp getSettingsCommands) "This help.")
    ]

setSettingsCommands :: CommandMap
setSettingsCommands = M.fromList [
        ("historyFile",       CommandDef (cmdSetField $ makeSetter setSettings setHistoryFile settings) "Set histroy file."),
        ("autoAddHistory",    CommandDef (cmdSetField $ makeSetter setSettings setAutoAddHistory settings) "Enable/disable add command to history."),
        ("maxRecursionDepth", CommandDef (cmdSetField $ makeSetter setSettings setMaxRecursionDepth settings) "Set maximum recursion depth."),
        ("help",              CommandDef (cmdHelp setSettingsCommands) "This help.")
    ]

initialProgramState :: Settings -> ProgramState
initialProgramState settings = ProgramState {settings = settings, cddb = emptyCDDB, currentRules = [], isNotSaved = True, currentTemplate = Nothing}

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

cmdRun :: Command
cmdRun args state = if null results
    then return $ Left "Nothing happened..."
    else do
        mapM_ printResult results
        return $ Right state
    where
        tree = read $ unwords args
        results = applyTree (cddb state) tree (maxRecursionDepth $ settings state)
        printResult result = do
            putStr "Score:"
            print (accumulatedScore result)
            putStrLn "Tree after run:"
            print (currentTree result)
            putStrLn "Maximum recursion depth:"
            print (recursionDepth result)
            putStrLn "Accumulated knowledge:"
            print (accumulatedKnowledge result)
            return ()

cmdDumpCDDB :: Command
cmdDumpCDDB [] state = do
    print (cddb state)
    return $ Right state
cmdDumpCDDB _ _ = errTooManyArguments

cmdShowRules :: Command
cmdShowRules [] state = do
    mapM_ (putStrLn . ruleDesc) (currentRules state)
    return $ Right state
cmdShowRules _ _ = errTooManyArguments

cmdNewRule :: Command
cmdNewRule [] state = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = [(newUUID, newRule)]}
cmdNewRule _ _ = errTooManyArguments

cmdClearRules :: Command
cmdClearRules [] state = return $ Right state {currentRules = []}
cmdClearRules _ _ = errTooManyArguments

cmdAddRule :: Command
cmdAddRule [] state = do
    Just newUUID <- nextUUID
    return $ Right state {currentRules = (newUUID, newRule) : currentRules state}
cmdAddRule _ _ = errTooManyArguments

cmdDeleteRule :: Command
cmdDeleteRule args state = return $ Right state {currentRules = filter isIdInIds $ currentRules state}
    where
        isIdInIds (ruleId, _) = elem ruleId $ catMaybes $ map fromString args

cmdWipeRule :: Command
cmdWipeRule args state = return $ Right state {cddb = deleteRulesToCDDB (cddb state) ids}
    where
        ids = catMaybes $ map fromString args

cmdWriteRules :: Command
cmdWriteRules [] state = return $ Right state {cddb = addRulesToCDDB (cddb state) (currentRules state)}
cmdWriteRules _ _ = errTooManyArguments

cmdRenewRules :: Command
cmdRenewRules [] state = do
    uuids <- replicateM (length crs) nextUUID
    return $ Right state {currentRules = zip (map fromJust uuids) crs}
    where
        crs = map snd $ currentRules state
cmdRenewRules _ _ = errTooManyArguments

cmdFilterRules :: Command
cmdFilterRules args state = printAndUpdateCurrentRules (boundRuleDesc tree) (mapSnd fst) rulesFound state
    where
        tree = read $ unwords args
        rulesFound = M.toList $ matchRulesAndFindPaths tree (rules $ cddb state)

cmdFindRules :: Command
cmdFindRules args state = printAndUpdateCurrentRules ruleDesc id rulesFound state
    where
        ids = mapMaybe fromString args
        rulesFound = mapMaybe (findCDDBRuleById $ cddb state) ids

printAndUpdateCurrentRules :: (a -> String) -> (a -> (RuleId, Rule)) -> [a] -> ProgramState -> IO (Either String ProgramState)
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

setMaxRecursionDepth :: Settings -> RecursionDepth -> Settings
setMaxRecursionDepth settings maxRecursionDepth = settings {maxRecursionDepth = maxRecursionDepth}

cmdQuit :: Command
cmdQuit = undefined

cmdHelp :: CommandMap -> Command
cmdHelp cmds args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds
    where
        addCommandHelp names key (CommandDef _ def) acc = if null names || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB [] state = return $ Right $ initialProgramState $ settings state
cmdCreateEmptyCDDB _ _ = errTooManyArguments

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
                Nothing -> return $ Left "Error reading file."
                Just cddb -> return $ Right $ updateStateWithDB state cddb fn

updateStateWithDB :: ProgramState -> CDDB -> FilePath -> ProgramState
updateStateWithDB state cddb fn = state {cddb = cddb, isNotSaved = False, settings = (settings state) {cddbFileName = Just fn}}

extractFileName :: Arguments -> ProgramState -> Either String String
extractFileName [] state = maybeToEither "File name is not specified" $ cddbFileName $ settings state
extractFileName [s] _ = Right s
extractFileName _ _ = Left "Should be only one file name"

errTooManyArguments :: IO (Either String ProgramState)
errTooManyArguments = return $ Left "Too many arguments."