{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.SyntacticTree where

import GHC.Generics
import Data.Aeson (ToJSON, FromJSON)

-- Tree like "S" ["NP" ["DET" [Word "the"], "N" [Word "cat"]], "VP" ["VPP" [Word "chase"], "NP" ["DET" [Word "a"], "N" [Word "mouse"]]]]
data SyntacticTree = Tag TagId [SyntacticTree]
    | Word String
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Tree like S [*, NP, VP [VPP ["chase"]], *]
data FilterExpression = Asterix
    | FilterTag TagId [FilterExpression]
    | FilterWord String
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

type TagId = String

matchFilter :: SyntacticTree -> FilterExpression -> Bool
matchFilter _ Asterix = True
matchFilter (Tag id ts) (FilterTag fid fs) = id == fid && matchFilter' ts fs
matchFilter (Word s) (FilterWord fs) = s == fs
matchFilter _ _ = False

matchFilter' :: [SyntacticTree] -> [FilterExpression] -> Bool
matchFilter' [] [] = True
matchFilter' _ [] = False
matchFilter' [] (Asterix: sfs) = matchFilter' [] sfs
matchFilter' [] _ = False
matchFilter' ts@(t: sts) fs@(Asterix: sfs) = matchFilter' sts fs || matchFilter' ts sfs
matchFilter' (t: sts) (f: sfs) = matchFilter t f &&  matchFilter' sts sfs