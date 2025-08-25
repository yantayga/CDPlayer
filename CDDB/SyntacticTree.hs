{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.SyntacticTree where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)
import Data.List (intercalate)

type TagId = String

-- Tree like "S" ["NP" ["DET" [Word "the"], "N" [Word "cat"]], "VP" [Word "chase"], "NP" ["DET" [Word "a"], "N" [Word "mouse"]]]
data SyntacticTree = Tag TagId [SyntacticTree]
    | Word TagId String
    deriving (Eq, Generic, ToJSON, FromJSON)

instance Show SyntacticTree where
    show :: SyntacticTree -> String
    show (Tag id ts) = id ++ " [" ++ (intercalate ", " $ map show ts) ++ "]"
    show (Word id w) = id ++ "(" ++ show w ++ ")"

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
