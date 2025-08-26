{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Types where

import qualified Data.Map as M

import CDDB.Types

type VariableStates = M.Map VariableName Constant
