{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Command.Errors where

errTooManyArguments   = Left "Too many arguments"
errNotEnoughArguments = Left "Not enough arguments"
errOutOfRange         = Left "Index out of range"