{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Tree.Syntax where

import GHC.Generics

data XBTree = XBTree0 TagId
    | XBTree1 TagId XBTree
    | XBTree2 TagId XBTree XBTree
    | XBWord TagId String WordProperties
    deriving (Eq, Show, Generic)
    
data XBarDirection = Left | Right deriving (Eq, Show, Generic)

type XBarPath = [XBarDirection]
