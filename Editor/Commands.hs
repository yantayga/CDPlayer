{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands (runMainCommand, initialProgramState, isNotSaved)  where

import CDDB.Types
import CDDB.Process

import Data.Time
import qualified Data.Map as M
import Data.List (intercalate, isInfixOf)
import Data.Either.Extra (maybeToEither)
import Data.Aeson (encode, decode, toJSON, fromJSON)
import qualified Data.ByteString.Lazy as B

import System.IO

data ProgramState = ProgramState {
        cddb :: CDDB,
        cddbFileName :: Maybe String,
        isNotSaved :: Bool,
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
        ("read", CommandDef (runCommand readCommands) "Read database field." Nothing),
        ("write", CommandDef (runCommand writeCommands) "Write database field.." Nothing),
        ("quit", CommandDef cmdQuit "Quit program." Nothing),
        ("test", CommandDef cmdTestErrMsg "Show test error message with arguments." Nothing)
    ]

readCommands :: CommandMap
readCommands = M.fromList [
        ("help", CommandDef (cmdHelp readCommands) "This help." Nothing),
        ("name", CommandDef (cmdReadCDDBField name) "Read database name." Nothing),
        ("comment", CommandDef (cmdReadCDDBField comment) "Read database comment." Nothing),
        ("version", CommandDef (cmdReadCDDBField version) "Read database version." Nothing),
        ("date", CommandDef (cmdReadCDDBField date) "Read database date." Nothing)
    ]

writeCommands :: CommandMap
writeCommands = M.fromList [
        ("help", CommandDef (cmdHelp writeCommands) "This help." Nothing),
        ("name", CommandDef (cmdWriteCDDBField set_name) "Write database name." Nothing),
        ("comment", CommandDef (cmdWriteCDDBField set_comment) "Write database comment." Nothing),
        ("version", CommandDef (cmdWriteCDDBField set_version) "Write database version." Nothing),
        ("date", CommandDef (cmdWriteCDDBField set_date) "Write database date." Nothing)
    ]

initialProgramState :: ProgramState
initialProgramState = ProgramState {cddb = emptyCDDB, cddbFileName = Nothing, isNotSaved = True, currentRule = Nothing}

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

cmdReadCDDBField :: Show a => (CDDB -> a) -> Command
cmdReadCDDBField accessor [] state = return $ Left $ show $ accessor $ cddb state
cmdReadCDDBField _ _ _ = return $ Left "Too many arguments"

cmdWriteCDDBField :: Read a => (CDDB -> a -> CDDB) -> Command
cmdWriteCDDBField accessor (val:[]) state = return $ Right $ state {cddb = accessor (cddb state) (read val)}
cmdWriteCDDBField _ [] _ = return $ Left "Not enough arguments"
cmdWriteCDDBField _ _ _ = return $ Left "Too many arguments"

set_name :: CDDB -> String -> CDDB
set_name cddb name = cddb {name = name}

set_comment :: CDDB -> String -> CDDB
set_comment cddb comment = cddb {comment = comment}

set_version :: CDDB -> Integer -> CDDB
set_version cddb version = cddb {version = version}

set_date :: CDDB -> UTCTime -> CDDB
set_date cddb date = cddb {date = date}


cmdTestErrMsg :: Command
cmdTestErrMsg args _= return $ Left $ "TEST ERROR MESSAGE: " ++ intercalate " " args

cmdQuit :: Command
cmdQuit = undefined

cmdHelp :: CommandMap -> Command
cmdHelp cmds args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\nType <command> help for command help\n" cmds
    where
        addCommandHelp names key (CommandDef _ def subs) acc = if names == [] || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB args state = return $ Right initialProgramState

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
