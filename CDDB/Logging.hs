{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Logging where

type Logs = [(LogLevel, LogString)]
type LogString = String

data LogLevel = Info | Warning | Debug | Error deriving (Eq, Enum, Ord, Show)

emptyLog :: Logs
emptyLog = []

addLogLine :: LogLevel -> LogString -> Logs -> Logs
addLogLine ll s ls = (ll, s):ls

printLogs :: Logs -> IO ()
printLogs = mapM_ print
