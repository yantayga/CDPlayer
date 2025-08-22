{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Types where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.Map

data CDDB = CDDB Name Version Date PrimitiveTemplates Rules Knowledge deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype PrimitiveTemplates = PrimitiveTemplates (Map Name FieldDefinitions) deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FieldDefinitions = FieldDefinitions [Name] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Rules = Rules [Rule] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Rule = Rule Comment Score Match Locals Conditions Actions deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Locals = Locals [VariableDef] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data VariableDef = VariableDef Name Expression deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Conditions = Conditions [Expression] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FieldVariables = FieldVariables [Expression] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Actions = Actions [Action] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Primitive = Primitive Name FieldVariables deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Action = Stop
    | Delete VariableName
    | AddFact Primitive
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Knowledge = Knowledge [Fact] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Fact = Fact Name FieldConstants deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FieldConstants = FieldConstants [Constant] deriving (Eq, Show, Generic, ToJSON, FromJSON)

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
type Version = Integer
type Score = Double
type Date = Maybe UTCTime
type Match = String

