{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Editor.Settings where

import GHC.Generics
import Data.Aeson

data Settings = Settings deriving (Eq, Show, Generic, ToJSON, FromJSON)