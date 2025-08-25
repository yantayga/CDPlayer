{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.SyntacticTree where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON)
import Data.List (intercalate)
import Control.Applicative

--import Text.Parsec hiding(choice, (<|>))
import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Parsers

type TagId = String

-- Tree like "S" ["NP" ["DET" [Word "the"], "N" [Word "cat"]], "VP" [Word "chase"], "NP" ["DET" [Word "a"], "N" [Word "mouse"]]]
data SyntacticTree = Tag TagId [SyntacticTree]
    | Word TagId String
    deriving (Eq, Generic, ToJSON, FromJSON)

instance Show SyntacticTree where
    show :: SyntacticTree -> String
    show (Tag id ts) = id ++ " [" ++ (intercalate ", " $ map show ts) ++ "]"
    show (Word id w) = id ++ "(" ++ show w ++ ")"

instance Read SyntacticTree where
    readPrec = choice [readTag Tag, readWord Word]
    readListPrec = readListPrecDefault

-- Tree like S [*, NP, VP [VPP ["chase"]], *]
data FilterExpression = Asterisk
    | FilterTag TagId [FilterExpression]
    | FilterWord TagId String
    deriving (Eq, Generic, ToJSON, FromJSON)

instance Show FilterExpression where
    show :: FilterExpression -> String
    show Asterisk = "*"
    show (FilterTag id ts) = id ++ "[" ++ (intercalate ", " $ map show ts) ++ "]"
    show (FilterWord id w) = id ++ "(" ++ show w ++ ")"

instance Read FilterExpression where
    readPrec = choice [readAsterisk, readTag FilterTag, readWord FilterWord]
        where
            readAsterisk = do
                L.Symbol "*" <- lexP
                return Asterisk
    readListPrec = readListPrecDefault


readTag c = do
    L.Ident n <- lexP
    l <- step readListPrec
    return $ c n l

readWord c = do
    L.Ident n <- lexP
    paren
        ( do
            L.String s <- lexP
            return $ c n s
        )