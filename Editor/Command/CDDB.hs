{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.CDDB where

import Data.Time (UTCTime(..), getCurrentTime)
import Data.Either.Extra (maybeToEither)
import Data.Aeson (encode, decode, toJSON)
import qualified Data.ByteString.Lazy as B

import System.IO

import CDDB.CDDB

import Editor.Command.Types
import Editor.Command.Common
import Editor.Command.Settings

-- CDDB Fields setters
setName :: CDDB -> String -> CDDB
setName cddb name = cddb {name = name}

setComment :: CDDB -> String -> CDDB
setComment cddb comment = cddb {comment = comment}

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
cmdDumpCDDB _ _ = errTooManyArguments

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
