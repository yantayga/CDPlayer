{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.VariableDefs where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Expression.Expression

data VariableDef = VariableDef Name Expression deriving (Eq, Generic, ToJSON, FromJSON)

type VariableDefs = [VariableDef]

instance Show VariableDef where
    show :: VariableDef -> String
    show (VariableDef name ex) = name ++ " = " ++ show ex

instance Read VariableDef where
    readPrec = do
        L.Ident name <- lexP
        expectP (L.Symbol "=")
        ex <- step readPrec
        return $ VariableDef name ex
                