{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Templates where

import Data.Map

import CDDB.Types

type PrimitiveTemplates = Map Name FieldDefinitions

type FieldDefinitions = [Name]
