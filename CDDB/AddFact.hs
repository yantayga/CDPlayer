{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.AddFact where

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

data AddFact = AddFact Name VariableDefs deriving (Generic, Eq)

instance ToJSON AddFact where
    toJSON t = toJSON $ show t

instance FromJSON AddFact where
   parseJSON = tryParseJSON

instance Show AddFact where
    show :: AddFact -> String
    show (AddFact name fvs) = name ++ " " ++ show fvs

instance Read AddFact where
    readPrec = do
                L.Ident s <- lexP
                es <- readListPrec
                return $ AddFact s es
