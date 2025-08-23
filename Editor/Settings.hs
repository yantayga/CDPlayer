{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Settings where

import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import System.IO

data Settings = Settings deriving (Eq, Show, Generic, ToJSON, FromJSON)

decodeSettings :: B.ByteString -> Maybe Settings
decodeSettings s = decode s

readSettings :: IO (Settings)
readSettings = do
    fileContent <- B.readFile "editorSettings.json"
    case decodeSettings fileContent of
        Nothing -> return Settings
        Just s -> return s

writeSettings :: Settings -> IO ()
writeSettings settings = do
    B.writeFile "editorSettings.json" $  encode (toJSON settings)


