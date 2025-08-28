{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.CDDB where

import qualified Data.Map as M
import Data.Time (UTCTime(..), getCurrentTime)
import Data.Either.Extra (maybeToEither)
import Data.Aeson (encode, decode, toJSON)
import qualified Data.ByteString.Lazy as B

import System.IO

import CDDB.CDDB

import Editor.Command.Types
import Editor.Command.Common
import Editor.Command.Errors
import Editor.Command.Help
import Editor.Command.Settings

getCDDBCommands :: CommandMap
getCDDBCommands = M.fromList [
        ("help",     CommandDef (cmdHelp getCDDBCommands) "This help."),
        ("name",     CommandDef (cmdGetField $ makeGetter cddb name) "Get database name."),
        ("comment",  CommandDef (cmdGetField $ makeGetter cddb cddbcomment) "Get database comment."),
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

-- CDDB Fields setters
setName :: CDDB -> String -> CDDB
setName cddb name = cddb {name = name}

setComment :: CDDB -> String -> CDDB
setComment cddb comment = cddb {cddbcomment = comment}

setVersion :: CDDB -> Integer -> CDDB
setVersion cddb version = cddb {version = version}

setDate :: CDDB -> UTCTime -> CDDB
setDate cddb date = cddb {date = date}

setCDDB :: ProgramState -> CDDB -> ProgramState
setCDDB state cddb = state {cddb = cddb}

-- Commands
cmdDumpCDDB :: Command
cmdDumpCDDB [] state = do
    print (cddb state)
    return $ Right state
cmdDumpCDDB _ _ = return errTooManyArguments

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB [] state = return $ Right $ initialProgramState $ settings state
cmdCreateEmptyCDDB _ _ = return errTooManyArguments

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
