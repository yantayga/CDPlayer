module CDDB.JSON where

import Data.Aeson
import CDDB.Types

instance FromJSON CDDB
instance ToJSON CDDB

instance FromJSON PrimitiveTemplates
instance ToJSON PrimitiveTemplates

instance FromJSON FieldDefinitions
instance ToJSON FieldDefinitions

instance FromJSON Rules
instance ToJSON Rules

instance FromJSON Rule
instance ToJSON Rule

instance FromJSON Locals
instance ToJSON Locals

instance FromJSON VariableDef
instance ToJSON VariableDef

instance FromJSON Contitions
instance ToJSON Contitions

instance FromJSON Primitives
instance ToJSON Primitives

instance FromJSON Primitive
instance ToJSON Primitive

instance FromJSON FieldValues
instance ToJSON FieldValues

instance FromJSON Actions
instance ToJSON Actions

instance FromJSON Action
instance ToJSON Action

instance FromJSON Expression
instance ToJSON Expression

instance FromJSON UnOp
instance ToJSON UnOp

instance FromJSON BinOp
instance ToJSON BinOp

