{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.CDDB where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Time (UTCTime(..))
import Data.Map (empty)
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Data.UUID (UUID)

import CDDB.Types
import CDDB.Rules
import CDDB.Templates

data CDDB = CDDB {
        name :: Name,
        comment :: Comment,
        version :: Integer,
        date :: UTCTime,
        templates :: PrimitiveTemplates,
        rules :: Rules,
        kn :: Knowledge
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

emptyCDDB :: CDDB
emptyCDDB = CDDB {
        name = "",
        comment = "",
        version = 1,
        date = UTCTime (fromOrdinalDate 0 0) 0,
        templates = empty,
        rules = empty,
        kn = []
    }

findCDDBRuleById :: CDDB -> UUID -> Maybe (RuleId, Rule)
findCDDBRuleById cddb = findRuleById (rules cddb)