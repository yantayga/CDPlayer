{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Actions where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.JSON

type Actions = [Action]

data Action = Stop
    | Delete [VariableName]
    | AddFact Name FieldVariables
    deriving (Eq, Generic)

instance ToJSON Action where
    toJSON t = toJSON $ show t

instance FromJSON Action where
   parseJSON = tryParseJSON

instance Show Action where
    show :: Action -> String
    show Stop = "stop"
    show (Delete vns) = "delete" ++ unwords vns
    show (AddFact name fvs) = "addFact " ++ name

instance Read Action where
    readPrec = choice [readStop, readDelete, readAddFact]
        where
            readStop = do
                expectP (L.Ident "stop")
                return Stop
            readDelete = do
                expectP (L.Ident "delete")
                vns <- step $ spaceList readPrec :: ReadPrec [VariableName]
                return $ Delete vns
            readAddFact = do
                expectP (L.Ident "addFact")
                L.Punc " " <- lexP
                L.Ident s <- lexP
                return $ AddFact s []

-- Taken form GHC.Read 'list'
spaceList :: ReadPrec a -> ReadPrec [a]
spaceList readx = do
    listRest +++ listNext
    where
        listRest = do
            L.Punc " " <- lexP
            listNext
        listNext = do
            x  <- reset readx
            xs <- listRest
            return (x:xs)
