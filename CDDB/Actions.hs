{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Actions where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Expression.Expression
import CDDB.Expression.VariableDefs
import CDDB.Parser
import CDDB.JSON

type Actions = [Action]

data Action = Stop
    | Delete [VariableName]
    | AddFact Name VariableDefs
    deriving (Eq, Generic)

instance ToJSON Action where
    toJSON t = toJSON $ show t

instance FromJSON Action where
   parseJSON = tryParseJSON

instance Show Action where
    show :: Action -> String
    show Stop = "stop"
    show (Delete vns) = "delete " ++ unwords vns
    show (AddFact name fvs) = "addFact " ++ name ++ " " ++ show fvs

instance Read Action where
    readPrec = choice [readStop, readDelete, readAddFact]
        where
            readStop = do
                expectP (L.Ident "stop")
                return Stop
            readDelete = do
                expectP (L.Ident "delete")
                vns <- spaceList
                return $ Delete vns
            readAddFact = do
                expectP (L.Ident "addFact")
                L.Ident s <- lexP
                es <- readListPrec
                return $ AddFact s es

