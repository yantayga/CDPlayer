{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Parser where

import GHC.Read
import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

spaceList :: ReadPrec [String]
spaceList = do
    L.Ident x  <- lexP
    xs <- spaceList +++ return []
    return (x:xs)

-- Taken form GHC.Read 'list'
cbList :: ReadPrec a -> ReadPrec [a]
cbList readx = do
    expectP (L.Punc "{")
    listRest False +++ listNext
    where
        listRest started = do
            L.Punc c <- lexP
            case c of
                "}"           -> return []
                "," | started -> listNext
                _             -> pfail
        listNext = do
            x  <- reset readx
            xs <- listRest True
            return (x:xs)