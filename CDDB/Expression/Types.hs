{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Types where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as M

import CDDB.Types
import CDDB.Tree.Syntax
import CDDB.Expression.UnOps

type VariableStates = M.Map VariableName Constant

data VariableDef = VariableDef Name Expression deriving (Eq, Show, Generic, ToJSON, FromJSON)

type FieldVariables = [Expression]

data Expression = Constant Constant
    | Variable VariableName
    | UnOp UnOp Expression
    | BinOp BinOp Expression Expression
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Constant = Error
    | Null
    | CBoolean Bool
    | CInteger Integer
    | CDouble Double
    | CString String
    | CTreePart TreePath
    | CTemplate Name
    | CType Name
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BinOp = Plus | Minus | Divide | Multiply
    | IsA | IsNotA
    | Dot
    deriving (Eq, Show, Generic, ToJSON, FromJSON)
