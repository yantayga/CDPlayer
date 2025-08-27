{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.UnOps where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

data UnOp = IsNull | IsNotNull | UnaryMinus deriving (Eq, Generic, ToJSON, FromJSON)

instance Show UnOp where
    show :: UnOp -> String
    show IsNull = "isNull"
    show IsNotNull = "isNotNull"
    show UnaryMinus = "-"

instance Read UnOp where
    readPrec = readIsNull <++ readIsNotNull <++ readUnaryMinus
        where
            readIsNull = do
                L.Ident "isNull" <- lexP
                return IsNull
            readIsNotNull = do
                L.Ident "isNotNull" <- lexP
                return IsNotNull
            readUnaryMinus = do
                L.Symbol "-" <- lexP
                return UnaryMinus
