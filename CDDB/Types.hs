{-# LANGUAGE DeriveGeneric #-}

module CDDB.Types where

import GHC.Generics
import Data.Time
import Data.Map


data CDDB = CDDB Name Version Date PrimitiveTemplates Rules deriving (Eq, Show, Generic)

newtype PrimitiveTemplates = PrimitiveTemplates (Map Name FieldDefinitions) deriving (Eq, Show, Generic)

newtype FieldDefinitions = FieldDefinitions [Name] deriving (Eq, Show, Generic)

newtype Rules = Rules [Rule] deriving (Eq, Show, Generic)

data Rule = Rule Comment Score Match Locals Contitions Primitives Actions deriving (Eq, Show, Generic)

newtype Locals = Locals [VariableDef] deriving (Eq, Show, Generic)

data VariableDef = VariableDef Name Nullable Name deriving (Eq, Show, Generic)

newtype Contitions = Contitions [Expression] deriving (Eq, Show, Generic)

newtype Primitives = Primitives [Primitive] deriving (Eq, Show, Generic)

data Primitive = Primitive Name FieldValues deriving (Eq, Show, Generic)

newtype FieldValues = FieldValues [Name] deriving (Eq, Show, Generic)

newtype Actions = Actions [Action] deriving (Eq, Show, Generic)

data Action = Stop
    | Delete Name
    deriving (Eq, Show, Generic)

data Expression = Variable Name
    | UnOp UnOp Expression
    | BinOp Name Expression Expression
    deriving (Eq, Show, Generic)

data UnOp = IsNull | IsNotNull | UnaryMinus deriving (Eq, Show, Generic)

data BinOp = Plus | Minus | Divide | Multiply 
    | IsA | IsNotA
    | Dot
    deriving (Eq, Show, Generic)

-- Aliases
type Name = String
type Comment = String
type Version = Integer
type Score = Double
type Date = Maybe UTCTime
type Match = String
type Nullable = Bool

