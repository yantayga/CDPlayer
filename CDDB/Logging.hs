{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Logging where

type Logs = String
type LogString = String

data LogLevel = Info | Warning | Debug deriving (Eq, Enum, Ord, Show)

addLogLine :: LogLevel -> LogString -> Logs -> Logs
addLogLine ll s ls = show ll ++ ":" ++ ls ++ "\n" ++ s

printLogs :: Logs -> IO ()
printLogs = print
