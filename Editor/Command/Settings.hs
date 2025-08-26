{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Settings where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import qualified System.Console.Haskeline as HL

import Control.Monad.Catch (catch, SomeException)

import CDDB.Tree.Syntax
import CDDB.Runner

data Settings = Settings {
        cddbFileName :: Maybe FilePath,
        cddbTree :: Maybe SyntacticTree,
        historyFile :: String,
        autoAddHistory :: Bool,
        maxRecursionDepth :: RecursionDepth
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Field setters
setHistoryFile :: Settings -> String -> Settings
setHistoryFile settings historyFile = settings {historyFile = historyFile}

setAutoAddHistory :: Settings -> Bool -> Settings
setAutoAddHistory settings autoAddHistory = settings {autoAddHistory = autoAddHistory}

setMaxRecursionDepth :: Settings -> RecursionDepth -> Settings
setMaxRecursionDepth settings maxRecursionDepth = settings {maxRecursionDepth = maxRecursionDepth}

-- Haskelline stuff
haskelinePrefsFromSettings :: Settings -> HL.Prefs
haskelinePrefsFromSettings _ = HL.defaultPrefs

haskelineSettionsFromSettings :: Settings ->  HL.Settings IO
haskelineSettionsFromSettings settings = HL.defaultSettings {
        HL.historyFile = Just $ historyFile settings,
        HL.autoAddHistory = autoAddHistory settings
    }

defalultSettings :: Settings
defalultSettings = Settings {
        cddbFileName = Nothing,
        cddbTree = Nothing,
        historyFile = ".cddb_history",
        autoAddHistory = True,
        maxRecursionDepth = 10
    }

readSettings :: IO Settings
readSettings = do
    fileContent <- flip catch exceptonHandler $ B.readFile settingsFilename
    case (decode :: B.ByteString -> Maybe Settings) fileContent of
        Nothing -> return defalultSettings
        Just s -> return s
    where
        exceptonHandler :: SomeException -> IO B.ByteString
        exceptonHandler _ = return B.empty

writeSettings :: Settings -> IO ()
writeSettings settings = do
    B.writeFile settingsFilename $  encode (toJSON settings)

settingsFilename :: String
settingsFilename = ".editor_state.json"

