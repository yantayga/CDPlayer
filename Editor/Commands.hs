{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Commands where

import CDDB.Types
import CDDB.Process

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
initialProgramState = ProgramState {cddb = emptyCDDB, cddbFileName = Nothing, isNotSaved = True, currentRule = Nothing}

runCommand :: String -> ProgramState -> IO (Either String ProgramState)
runCommand cmd state =
    case words cmd of
        [] -> return $ Left "Empty command"
        (cmdName: args) ->
            case M.lookup cmdName commands of
                Nothing -> return $ Left $ "Command '" ++ cmdName ++ "' not found. Possible variants: " ++ findMostSimilar cmdName
                Just (CommandDef fn _) -> fn args state
    where
        findMostSimilar cmdName = intercalate " " $ (filter . isInfixOf) cmdName $ M.keys commands

cmdTestErrMsg :: Command
cmdTestErrMsg args _= return $ Left $ "TEST ERROR MESSAGE: " ++ intercalate " " args

cmdHelp :: Command
cmdHelp args _ = return $ Left $ M.foldrWithKey (addCommandHelp args) "Commands:\n" commands
    where
        addCommandHelp names key (CommandDef _ def) acc = if names == [] || elem key names then acc ++ "\n" ++ key ++ ":\n\t" ++ def else ""

cmdCreateEmptyCDDB :: Command
cmdCreateEmptyCDDB args state = return $ Right initialProgramState

cmdSaveCDDB :: Command
cmdSaveCDDB args state = do
    case extractFileName args state of
        Left errMsg -> return $ Left errMsg
        Right fn -> do
            B.writeFile fn $ encode (toJSON $ cddb state)
            return $ Right state {isNotSaved = False}

cmdLoadCDDB :: Command
cmdLoadCDDB args state = do
    case extractFileName args state of
        Left errMsg -> return $ Left errMsg
        Right fn -> do
            handle <- openFile fn ReadWriteMode
            fileContent <- B.hGetContents handle
            case decodeCDDB fileContent of
                Nothing -> return $ Left "Error reading file "
                Just s -> return $ Right state {isNotSaved = False}

decodeCDDB :: B.ByteString -> Maybe CDDB
decodeCDDB s = decode s

extractFileName :: Arguments -> ProgramState -> Either String String
extractFileName [] state = maybeToEither "File name is not specified" $ cddbFileName state
extractFileName (s:[]) _ = Right s
extractFileName _ _ = Left "Should be only one file name"
