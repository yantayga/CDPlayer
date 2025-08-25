{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Process where

import Data.Map (fromList)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)

import CDDB.Types

emptyCDDB :: CDDB
emptyCDDB = CDDB {
        name = "",
        comment = "",
        version = 1,
        date = UTCTime (fromOrdinalDate 0 0) 0,
        templates = fromList [],
        rules = [],
        kn = []
    }


