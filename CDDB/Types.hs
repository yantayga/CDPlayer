module CDDB.Types where

import Text.Parsec.Indent
import Data.Time

type IParser a = IndentParser String () a

type Term = String
data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)

type Name = String
type Version = Integer
type Date = Maybe UTCTime

type Comment = String
type Score = Double
type Match = String
type Actions = String
data Rule = Rule Comment Score Match [Taxonomy] Actions deriving (Eq, Show)

data Rules = Rules [Rule] deriving (Eq, Show)

data Primitive = Primitive Name [Name] deriving (Eq, Show)

data Primitives = Primitives [Primitive] deriving (Eq, Show)

data CDDB = CDDB Name Version Date Primitives Rules deriving (Eq, Show)