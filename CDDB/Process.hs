{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Process where

import Data.Map (fromList)
import Data.Time
import Data.Time.Calendar.OrdinalDate

import CDDB.Types

emptyCDDB :: CDDB
emptyCDDB = CDDB "" 1 (UTCTime (fromOrdinalDate 0 0) 0) (PrimitiveTemplates $ fromList []) (Rules []) (Knowledge [])

updateCDDBDate :: CDDB -> UTCTime -> CDDB
updateCDDBDate cddb date = cddb {date = date}