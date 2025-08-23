{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Types where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.Map

import CDDB.SyntacticTree

data CDDB = CDDB {
        name :: String,
        comment :: String,
        version :: Integer,
        date :: UTCTime,
        templates :: PrimitiveTemplates,
        rules :: Rules,
        kn :: Knowledge
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

type PrimitiveTemplates = Map Name FieldDefinitions

type FieldDefinitions = [Name]

type Rules = [Rule]

data Rule = Rule Comment Score FilterExpression Locals Conditions Actions deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Locals = [VariableDef]

data VariableDef = VariableDef Name Expression deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Conditions = [Expression]

type FieldVariables = [Expression]

type Actions = [Action]

data Primitive = Primitive Name FieldVariables deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Action = Stop
    | Delete VariableName
    | AddFact Primitive
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
    | CType Name
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UnOp = IsNull | IsNotNull | UnaryMinus deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BinOp = Plus | Minus | Divide | Multiply
    | IsA | IsNotA
    | Dot
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Aliases
type Name = String
type VariableName = String
type Comment = String
type Score = Double
type Match = String

