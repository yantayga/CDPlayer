{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module CDDB.Types where

import GHC.Generics
import Data.Aeson
import Data.Time
import Data.Map


data CDDB = CDDB Name Version Date PrimitiveTemplates Rules Knowledge deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype PrimitiveTemplates = PrimitiveTemplates (Map Name FieldDefinitions) deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FieldDefinitions = FieldDefinitions [Name] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Rules = Rules [Rule] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Rule = Rule Comment Score Match Locals Contitions Primitives Actions deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Locals = Locals [VariableDef] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data VariableDef = VariableDef Name Nullable Name deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Contitions = Contitions [Expression] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Primitives = Primitives [Primitive] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Primitive = Primitive Name FieldValues deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype FieldValues = FieldValues [Name] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Actions = Actions [Action] deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Action = Stop
    | Delete VariableName
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Expression = Variable VariableName
    | StringValue String
    | IntegerValue Integer
    | DoubleValue Double
    | UnOp UnOp Expression
    | BinOp BinOp Expression Expression
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UnOp = IsNull | IsNotNull | UnaryMinus deriving (Eq, Show, Generic, ToJSON, FromJSON)

data BinOp = Plus | Minus | Divide | Multiply 
    | IsA | IsNotA
    | Dot
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Knowledge = Knowledge [Fact] deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype Fact = Fact [Primitive] deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Aliases
type Name = String
type VariableName = String
type Comment = String
type Version = Integer
type Score = Double
type Date = Maybe UTCTime
type Match = String
type Nullable = Bool

