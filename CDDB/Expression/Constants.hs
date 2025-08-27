{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Constants where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Tree.Syntax

data Constant = Error
    | Null
    | CBoolean Bool
    | CInteger Integer
    | CDouble Double
    | CString String
    | CTreePart TreePath
    | CTemplate Name
    | CType Name
    deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Constant where
    show :: Constant -> String
    show Error = "<error>"
    show Null = "null"
    show (CBoolean True) = "true"
    show (CBoolean False) = "false"
    show (CInteger i) = show i
    show (CDouble d) = show d
    show (CString s) = show s
    show (CTreePart _) = undefined
    show (CTemplate name) = "!" ++ name
    show (CType name) = "$" ++ name

instance Read Constant where
    readPrec = readNull <++ readTrue <++ readFalse <++ readInteger <++ readString
        where
            readNull = do
                L.Ident "null" <- lexP
                return Null
            readTrue = do
                L.Ident "true" <- lexP
                return $ CBoolean True
            readFalse = do
                L.Ident "false" <- lexP
                return $ CBoolean False
            readInteger = do
                L.Number num <- lexP
                case L.numberToInteger num of
                    Just n -> return $ CInteger n
                    Nothing -> return $ CDouble $ fromRational $ L.numberToRational num
            readString = do
                L.String s <- lexP
                return $ CString s
