{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Settings where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import System.IO
import qualified System.Console.Haskeline as HL

data Settings = Settings {
        historyFile :: String,
        autoAddHistory :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

haskelinePrefsFromSettings :: Settings -> HL.Prefs
haskelinePrefsFromSettings _ = HL.defaultPrefs

haskelineSettionsFromSettings :: Settings ->  HL.Settings IO
haskelineSettionsFromSettings settings = HL.defaultSettings {
        HL.historyFile = Just $ historyFile settings,
        HL.autoAddHistory = autoAddHistory settings
    }

defalultSettings :: Settings
defalultSettings = Settings {
        historyFile = ".cddb_history",
        autoAddHistory = True
    }

decodeSettings :: B.ByteString -> Maybe Settings
decodeSettings s = decode s

readSettings :: IO (Settings)
readSettings = do
    handle <- openFile "editorSettings.json" ReadWriteMode
    fileContent <- B.hGetContents handle
    case decodeSettings fileContent of
        Nothing -> return defalultSettings
        Just s -> return s

writeSettings :: Settings -> IO ()
writeSettings settings = do
    B.writeFile "editorSettings.json" $  encode (toJSON settings)


