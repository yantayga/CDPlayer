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
