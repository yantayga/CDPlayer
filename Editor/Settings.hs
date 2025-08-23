{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Settings where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import System.IO
import qualified System.Console.Haskeline as HL
import Control.Monad.Catch (catch, SomeException)

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

readSettings :: IO (Settings)
readSettings = do
    fileContent <- (flip catch) exceptonHandler $ B.readFile "editorSettings.json"
    case (decode :: B.ByteString -> Maybe Settings) fileContent of
        Nothing -> return defalultSettings
        Just s -> return s
    where
        exceptonHandler :: SomeException -> IO B.ByteString
        exceptonHandler ex = return B.empty

writeSettings :: Settings -> IO ()
writeSettings settings = do
    B.writeFile "editorSettings.json" $  encode (toJSON settings)


