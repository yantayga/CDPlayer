module CDDB.Types where

import Text.Parsec.Indent
import Data.Time

type IParser a = IndentParser String () a

-- TODO: Remove
-------------------
type Term = String
data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)
-------------------

type Name = String
type Version = Integer
type Date = Maybe UTCTime
type Comment = String
type Score = Double
type Match = String
type Actions = String
type Further = String

type Primitives = [Taxonomy]
type Locals = [Taxonomy]
type Conditions = [Taxonomy]

data Rule = Rule Comment Score Match Further Locals Conditions Primitives Actions deriving (Eq, Show)

data Rules = Rules [Rule] deriving (Eq, Show)

data PrimitiveTemplate = PrimitiveTemplate Name [Name] deriving (Eq, Show)

data PrimitiveTemplates = PrimitiveTemplates [PrimitiveTemplate] deriving (Eq, Show)

data CDDB = CDDB Name Version Date PrimitiveTemplates Rules deriving (Eq, Show)