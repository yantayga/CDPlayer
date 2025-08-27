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
import CDDB.Expression.Types

type Knowledge = [Fact]

data Fact = Fact Name FieldConstants deriving (Eq, Show, Generic, ToJSON, FromJSON)

type FieldConstants = [Constant]

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

addTemplatesToCDDB :: CDDB -> [PrimitiveTemplate] -> CDDB
addTemplatesToCDDB cddb newTemplates = cddb {templates = foldl (flip addTemplate) (templates cddb) newTemplates}

deleteTemplatesFromCDDB :: CDDB -> [Name] -> CDDB
deleteTemplatesFromCDDB cddb ns = cddb {templates = deleteTemplates (templates cddb) ns}

findCDDBRuleById :: CDDB -> UUID -> Maybe (RuleId, Rule)
findCDDBRuleById cddb = findRuleById (rules cddb)

addRulesToCDDB :: CDDB -> [(RuleId, Rule)] -> CDDB
addRulesToCDDB cddb newRules = cddb {rules = addRules (rules cddb) newRules}

deleteRulesFromCDDB :: CDDB -> [RuleId] -> CDDB
deleteRulesFromCDDB cddb ids = cddb {rules = deleteRules (rules cddb) ids}
