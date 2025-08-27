{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.BinOps where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

data BinOp = Plus | Minus | Divide | Multiply
    | IsA | IsNotA
    | Dot
    deriving (Eq, Generic, ToJSON, FromJSON)

instance Show BinOp where
    show :: BinOp -> String
    show Plus = "+"
    show Minus = "-"
    show Divide = "/"
    show Multiply = "*"
    show IsA = "isA"
    show IsNotA = "isNotA"
    show Dot = "."

instance Read BinOp where
    readPrec = readPlus <++ readMinus <++ readDivide <++ readMultiply <++ readIsA <++ readIsNotA <++ readDot
        where
            readPlus = do
                L.Symbol "+" <- lexP
                return Plus
            readMinus = do
                L.Symbol "-" <- lexP
                return Minus
            readDivide = do
                L.Symbol "/" <- lexP
                return Divide
            readMultiply = do
                L.Symbol "*" <- lexP
                return Multiply
            readIsA = do
                L.Ident "isA" <- lexP
                return IsA
            readIsNotA = do
                L.Ident "isNotA" <- lexP
                return IsNotA
            readDot = do
                L.Symbol "." <- lexP
                return Dot

binOpPriority :: BinOp -> Int
binOpPriority Plus = 1
binOpPriority Minus = 1
binOpPriority Divide = 2
binOpPriority Multiply = 2
binOpPriority IsA = 2
binOpPriority IsNotA = 2
binOpPriority Dot = 3
