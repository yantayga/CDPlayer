{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Types where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M

import CDDB.Types
import CDDB.Templates
import CDDB.Expression.UnOps
import CDDB.Expression.BinOps
import CDDB.Expression.Constants
import CDDB.Expression.Expression

type VariableStates = M.Map VariableName Constant

