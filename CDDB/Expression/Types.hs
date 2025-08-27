{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Types where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M

import CDDB.Types
import CDDB.Expression.UnOps
import CDDB.Expression.BinOps
import CDDB.Expression.Constants
import CDDB.Expression.Expression

type VariableStates = M.Map VariableName Constant

data VariableDef = VariableDef Name Expression deriving (Eq, Show, Generic, ToJSON, FromJSON)

type FieldVariables = [Expression]



