{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.DeleteNodes where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Expression.Expression
import CDDB.Expression.VariableDefs
import CDDB.Expression.Expression
import CDDB.Parser
import CDDB.JSON

data DeleteNodes = DeleteNodes [VariableName] deriving (Generic, Eq)

instance ToJSON DeleteNodes where
    toJSON t = toJSON $ show t

instance FromJSON DeleteNodes where
   parseJSON = tryParseJSON

instance Show DeleteNodes where
    show :: DeleteNodes -> String
    show (DeleteNodes vs) = unwords vs

instance Read DeleteNodes where
    readPrec = do
        vs <- spaceList
        return $ DeleteNodes vs

