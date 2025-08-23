{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Process where

import Data.Map (fromList)

import CDDB.Types

emptyCDDB :: CDDB
emptyCDDB = CDDB "" 1 Nothing (PrimitiveTemplates $ fromList []) (Rules []) (Knowledge [])
