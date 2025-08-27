{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Types where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

type Name = String
type VariableName = String
type Comment = String

type Locals = [VariableDef]

data VariableDef = VariableDef Name Expression deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Conditions = [Expression]

type FieldVariables = [Expression]

data Primitive = Primitive Name FieldVariables deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Knowledge = [Fact]

data Fact = Fact Name FieldConstants deriving (Eq, Show, Generic, ToJSON, FromJSON)

type FieldConstants = [Constant]

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
    | CPrimitive Fact
    | CTreePart TreePath
    | CType Name
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UnOp = IsNull | IsNotNull | UnaryMinus deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BinOp = Plus | Minus | Divide | Multiply
    | IsA | IsNotA
    | Dot
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

type TreePos = Int
type TreePath = [TreePos]